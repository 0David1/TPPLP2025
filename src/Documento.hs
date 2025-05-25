module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio


foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc cVacio cTexto cLinea doc = case doc of
    Vacio     -> cVacio
    Texto s d -> cTexto s (rec d)
    Linea n d -> cLinea n (rec d)
  where rec = foldDoc cVacio cTexto cLinea

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

-- funcion auxiliar para manejar el caso en que se combinen dos textos
concatenar :: String -> Doc -> Doc
concatenar s1 d2 = case d2 of
  Vacio        -> texto s1
  Texto s2 doc -> Texto (s1++s2) doc
  Linea n doc  -> Texto s1 (Linea n doc)


(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 concatenar Linea d1

-- Sea Texto s d entonces:

-- s no debe ser el string vacio, se satisface pues sea `texto s` si s es el string vacio entonces la funcion texto retorna Vacio
-- por lo que es imposible obtener un Doc `Texto s d`con s siendo ""

-- s no debe contener saltos de linea, se satisface pues la funcion texto usa elem para buscar saltos de linea en el string s, en caso de haberlos retorna error

-- d debe ser Vacio o Linea i d', en el caso de que se realize d1 <+> d2, si d1 es Texto s1 Vacio:
-- en caso de que d2 sea otro Texto s2 combina sus string en un solo Texto (s1++s2) Vacio
-- en caso de que d2 sea Vacio entonces se devuelve d1
-- en caso de que d2 sea Linea se concatena a d1 retornando Texto s1 d2

-- sea Linea i d entonces:

-- i >= 0, se cumple puesto que la funcion linea retorna Linea 0 Vacio y en la funcion no modificamos el valor int asociado

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio Texto (Linea . (+i))

{-
Sea Texto s d entonces:
  s no debe ser el string vacio y s no debe contener saltos de linea, la funcion Texto mantiene estos dos invariantes.
  
  d debe ser Vacio o Linea i d', en este caso d solo puede ser Vacio siempre que usemos la funcion texto, la unica forma de que d sea Texto o Linea es concatenando con otro Doc
  y esto solo ocurre usando la funcion <+>, y en el ejercicio anterior vimos que dicha funcion mantiene este invariante por lo tanto se cumple

sea Linea i d entonces:
  i >= 0, se cumple pues la funcion linea genera una Linea 0 Vacio y su valor solo se modifica mediante indentar que toma valor i mayor a cero
  y ese valor se suma al valor que tenia previamente linea, que de ser cero sumo algo positivo, y en caso de que sea otro numero , por lo previamente mencionado
  positivo + positivo -> positivo

Sea Vacio:
  se devuelve Vacio.

-}

mostrar :: Doc -> String
mostrar = foldDoc "" (++) (\i d -> ("\n" ++ replicate i ' ') ++ d)

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)

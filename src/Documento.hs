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

-- foldDoc :: ... PENDIENTE: Ejercicio 1 ...
foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc cVacio cTexto cLinea documento = case documento of
    Vacio -> cVacio
    Texto text doc ->  cTexto text (rec doc)
    Linea n doc -> cLinea n (rec doc)
  where
    rec = foldDoc cVacio cTexto cLinea 

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

-- funcion auxiliar para manejar el caso en que se combinen dos textos
combinar :: String -> Doc -> Doc
combinar s1 d2 = case d2 of
  Vacio -> texto s1
  Texto s2 doc -> Texto (s1++s2) doc
  Linea n doc -> Texto s1 (Linea n doc)


(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 combinar Linea d1

-- Sea Texto s d entonces:

-- s no debe ser el string vacio, se satisface pues sea `texto s` si s es el string vacio entonces la funcion texto retorna Vacio
-- por lo que es imposible obtener un Doc de tipo `Texto s d`con s siendo ""

-- s no debe contener saltos de linea, se satisface pues la funcion texto usa elem para buscar saltos de linea en el string s, en caso de haberlos retorna error

-- d debe ser Vacio o Linea i d',

-- sea Linea i d entonces: i >= 0,

agregarIdentacion :: Int -> Int -> Doc -> Doc
agregarIdentacion agregar n = Linea (n+agregar)

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio Texto (agregarIdentacion i) 

-- 

generarEspacio :: Int -> String -> String
generarEspacio n s = "\n" ++ replicate n ' ' ++ s

mostrar :: Doc -> String
mostrar = foldDoc [] (++) generarEspacio

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
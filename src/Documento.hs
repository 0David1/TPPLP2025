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


-- Ejercicio 1
foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc cVacio cTexto cLinea doc = case doc of
    Vacio     -> cVacio
    Texto s d -> cTexto s (rec d)
    Linea n d -> cLinea n (rec d)
  where rec = foldDoc cVacio cTexto cLinea

-- Ejercicio 2

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 concatenar Linea d1

concatenar :: String -> Doc -> Doc
concatenar s1 d2 = case d2 of
  Vacio        -> texto s1
  Texto s2 doc -> Texto (s1++s2) doc
  Linea n doc  -> Texto s1 (Linea n doc)

-- Ejercicio 3
indentar :: Int -> Doc -> Doc
indentar n = foldDoc Vacio Texto (\m acc -> Linea (m + n) acc)


-- Ejercicio 4
mostrar :: Doc -> String
mostrar = foldDoc "" (++) (\i rec -> ("\n" ++ concat (replicate i " ")) ++ rec)



-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)

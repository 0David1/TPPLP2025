module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

-- Ejercicio 5
pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  TextoPP _ -> True
  IntPP _   -> True
  _         -> False   


-- Ejercicio 6
pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP l) = foldr (\e rec -> pponAtomico (snd e) && rec) True l

-- Ejercicio 7
intercalar :: Doc -> [Doc] -> Doc
intercalar _ []           = vacio
intercalar separador docs = foldr1 (\doc rec -> doc <+> separador <+> rec) docs


entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"


-- Ejercicio 8
aplanar :: Doc -> Doc
aplanar = texto . foldDoc "" (++) (\_ rec -> ' ':rec)

-- Ejercicio 9
pponADoc :: PPON -> Doc
pponADoc pp = case pp of
    TextoPP s  -> texto (show s)
    IntPP i    -> texto (show i)
    ObjetoPP l
        | pponObjetoSimple (ObjetoPP l) -> aplanar (entreLlaves (agregarDosPuntos l))
        | otherwise                     -> entreLlaves (agregarDosPuntos l)
  where agregarDosPuntos = foldr (\(x , y) rec -> intercalar (texto ": ") [texto (show  x), pponADoc y ] : rec) []


-- Es recursion estructural pues los casos base devuelven un valor fijo que no depende de la funcion pponADoc
-- y el caso recursivo no usa pponAdoc ni los 'y' en otro lado salvo en la expresion (pponADoc y)
-- ('y' representa a los PPON en cada tupla de la lista que vendrian siendo las subestructuras sobre las cuales se hace la recursion)

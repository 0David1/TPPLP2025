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
pponADoc = error "PENDIENTE: Ejercicio 9"

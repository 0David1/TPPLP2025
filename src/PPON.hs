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
  TextoPP _  -> True
  IntPP _    -> True
  ObjetoPP _ -> False   


-- Ejercicio 6
pponObjetoSimple :: PPON -> Bool
pponObjetoSimple p = case p of
  TextoPP _  -> True
  IntPP _    -> True
  ObjetoPP l -> foldr (\e rec -> pponAtomico (snd e) && rec) True l


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
pponADoc p = case p of
  TextoPP s      -> texto (show s)
  IntPP n        -> texto (show n)
  ObjetoPP lista -> if pponObjetoSimple (ObjetoPP lista) then aplanar (formatoLlaves lista)
                                                         else formatoLlaves lista
  where formatoLlaves = entreLlaves . map (\(x, rec) -> texto (show x ++ ": ") <+> pponADoc rec)

{-
  Es recursion primitiva pues:
    1. Los casos base, "Texto s" y "IntPP n", devuelven valores fijos.
    2. El caso recursivo opera sobre "lista" de "Objeto lista" e invoca pponADoc sobre la subestructura. 
-}


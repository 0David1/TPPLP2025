module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  TextoPP _  -> True
  IntPP _    -> True
  ObjetoPP _ -> False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP lista) = foldr (\dupla rec-> pponAtomico(snd dupla) && rec) True lista

intercalar :: Doc -> [Doc] -> Doc
intercalar _ []        = vacio
intercalar separador d = foldr1 (\doc rec -> doc <+> separador <+> rec) d

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

aplanar :: Doc -> Doc
aplanar = texto . foldDoc "" (++) (\_ d -> ' ' : d)

pponADoc :: PPON -> Doc
pponADoc p = case p of
  TextoPP s -> texto (show s)
  IntPP n -> texto (show n)
  ObjetoPP lista -> if pponObjetoSimple (ObjetoPP lista) then aplanar (formatoLlaves lista)
                                                       else formatoLlaves lista
  where formatoLlaves = entreLlaves . map (\(x, rec) -> texto (show x ++ ": ") <+> pponADoc rec)

-- Es recursion estructural pues los casos base devuelven un valor fijo que no depende de la funcion pponADoc
-- y el caso recursivo no usa pponAdoc ni la subestructura en otro lado salvo en la expresion (pponADoc rec)
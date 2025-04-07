module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pericles1 = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina1 = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams1 = ObjetoPP [("0", pericles1), ("1", merlina1)]

pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  TextoPP _ -> True
  IntPP _ -> True
  ObjetoPP _ -> False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP l) = foldr (\(s, p) d -> pponAtomico p && d) True l

intercalar :: Doc -> [Doc] -> Doc
intercalar separador = foldr (\d ds -> if ds /= vacio then d <+> separador <+> ds else d) vacio

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
aplanar = foldDoc vacio (\s d -> texto s <+> d) (\_ d -> texto " " <+> d)

pponADoc :: PPON -> Doc
pponADoc p = case p of
  TextoPP s -> texto (show s)
  IntPP n -> texto (show n)
  ObjetoPP l -> if not (pponObjetoSimple (ObjetoPP l)) then entreLlaves (formatoListaDoc l) 
                                                       else aplanar (entreLlaves (formatoListaDoc l))
  where formatoListaDoc :: (Show a) => [(a, PPON)] -> [Doc]
        formatoListaDoc = map (\(x, y) -> texto (show x ++ ": ") <+> pponADoc y)

-- Es recursion estructural pues los casos base devuelven un valor fijo que no depende de la funcion pponADoc
-- y el caso recursivo no usa pponAdoc ni los 'y' en otro lado salvo en la expresion (pponADoc y)
-- ('y' representa a los PPON en cada tupla de la lista que vendrian siendo las subestructuras sobre las cuales se hace la recursion)
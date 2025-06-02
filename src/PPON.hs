module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

--EJ5
pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  TextoPP _  -> True
  IntPP _    -> True
  ObjetoPP _ -> False

--EJ6
pponObjetoSimple :: PPON -> Bool
pponObjetoSimple p = case p of
  TextoPP _ -> True
  IntPP _ -> True
  ObjetoPP lista -> foldr (\dupla rec-> pponAtomico(snd dupla) && rec) True lista

--EJ7
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

--EJ8
aplanar :: Doc -> Doc
aplanar = texto . foldDoc "" (++) (\_ d -> ' ' : d)

--EJ9
pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s)
pponADoc (IntPP n) = texto (show n)
pponADoc (ObjetoPP lista) | pponObjetoSimple(ObjetoPP lista) = aplanar (formatoLlaves lista)
                          | otherwise = formatoLlaves lista
  where formatoLlaves = entreLlaves . map (\(x, rec) -> texto (show x ++ ": ") <+> pponADoc rec)

{-
  Es recursion primitiva ya que accedemos y predicamos sobre la subestructura, en otra funcion ademas de pponADoc termino recursivo sin ser la funcion recursiva.
-}
{-
  Es recursion primitiva pues:
    1. Los casos base devuelven valores fijos:
    2. el caso recursivo usa (g xs) y pponObjetoSimple en su definicion usa xs para pponAtomico que no es un llamado recursivo
    
    2. el caso recursivo usa (pponADoc sobre el recursivo) , y predica sobre Lista y objetoPP lista en varias partes 
    pponObjeto lista 
    iterativa no es porque no tiene acumulador.(el resultado recursivo es parametro) foldl
    primitiva porque devuelve el acumulador sin usarlo como paramtetro.

    el caso lo usas como parametro,   
-}
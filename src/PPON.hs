module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)


pponAtomico :: PPON -> Bool
pponAtomico (TextoPP _ )= True
pponAtomico (IntPP _) = True
pponAtomico _ = False



pponSegundo :: (String , PPON) -> PPON
pponSegundo (_, y) = y

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP lista) = foldr (\elemento rec-> pponAtomico(pponSegundo elemento) && rec) True lista



--chequear si es estructural
intercalar :: Doc -> [Doc] -> Doc
intercalar input [] = vacio
intercalar input (x: xs) =x <+> foldr(\d acc ->input<+>d<+> acc) vacio xs


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


hastaN :: String -> String
hastaN = foldr(\actual recursivo->if actual == '\n' then [] else actual:recursivo )""



{- foldDoc texto linea base Doc
-}
aplanar :: Doc -> Doc
aplanar = texto. foldDoc (\j acc -> hastaN(mostrar j)++ acc ) (\_ acc -> ' ': acc) ""
--aplanar =texto.foldDoc (\primero acc -> if head(mostrar primero) == '\n' then ' ':acc else hastaN(mostrar primero)++ acc) "" 


pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto ("\""++s++"\"")
pponADoc (IntPP i) = texto (show i)
pponADoc (ObjetoPP lista)|pponObjetoSimple (ObjetoPP lista) = aplanar (entreLlaves(foldr (\(primero , segundo) acc ->[intercalar(texto ": ") [texto ("\""++primero++"\""),pponADoc segundo ] ]++acc) [] lista))
  |otherwise = entreLlaves(foldr (\(primero , segundo) acc ->[intercalar(texto ": ") [texto ("\""++primero++"\""),pponADoc segundo ] ]++acc) [] lista)

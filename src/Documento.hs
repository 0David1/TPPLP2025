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


--es mas correcto tener una funcion por cada caso recursivo
foldDoc :: (Doc -> t -> t) -> (Doc -> t -> t) -> t -> Doc -> t
foldDoc fTexto fLinea base d =  case d of 
                                    Vacio -> base
                                    Texto s d -> fTexto( Texto s d) (recursivo d)
                                    Linea i d -> fLinea( Linea i d) (recursivo d)
                                where recursivo = foldDoc fTexto fLinea base




-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>




--preguntar si esta funcion rompe la consigna de que tiene que ser estructural solo foldDoc.
combinar :: Doc -> Doc -> Doc
combinar (Texto s d) (Texto s2 d3) = Texto (s ++ s2) d3
combinar (Texto s _) j = Texto s j



--justificar porque satisface el invariante.


{-
El variante de texto se mantiene ya que la funcion combinar en caso de combinar dos textos lo combina en 1, y recursivamente siempre que tengamos textos
seguidos se concatenaran en un solo Doc Texto (todos los texto seguidos concantenados) rec
entonces seguiremos concatenando hasta una linea o vacio.siempre partiendo de que combinamos dos docs validos
En el caso de linea no en ninguno de los casos se modifica el valor int asociado por lo que en caso de que partimos de docs que cumplen el invariante <+> 
no lo va a romper.

-}
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc combinar (\(Linea i _) acc -> Linea i acc) d2 d1



--indentar cumple con el invariante ya que solo afecta el constructor  linea, y como la funcion requiere que i sea mayor a cero , si parto de una estructura
--Doc valida entonces sumarle el parametro > 0 a una linea cuyo primer parametro es mayor a cero, nos devolvera un i >= 0.
indentar :: Int -> Doc -> Doc
indentar i | i > 0 =foldDoc (\(Texto s _) -> Texto s ) (\(Linea j _ ) -> Linea (j+i) ) Vacio 



mostrar :: Doc -> String 
mostrar = foldDoc (\(Texto s _) -> (s++)) (\(Linea n _) ->(++) ("\n" ++ [' '| _ <- [1..n]])) ""

-- | Función dada que imprime un documento en pantalla

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)

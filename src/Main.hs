module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 1" ~: testsEj1,
      "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 5" ~: testsEj5,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

--documentos para test
doc1, doc2, doc3, doc4, doc5, doc6 :: Doc
doc1 = vacio <+> vacio
doc2 = texto "a" <+> texto "b"
doc3 = texto "a" <+> (linea <+> texto "b")
doc4 = texto "a" <+> linea <+> texto "b" <+> texto "c" <+> linea <+> texto "d"
doc5 = texto "a" <+> linea <+> texto "b" <+> linea <+> linea <+> texto "c" <+> texto "d"
doc6 = texto "a" <+> texto "b" <+> texto "c" <+> linea <+> texto "d"
--foldDoc para contar la cantidad de Doc enlazados
foldDocCount = foldDoc 0 (\_ x -> 1 + x) (\_ x -> 1 + x)
--foldDoc para contar la cantidad de Doc que sean lineas
foldDocCountLinea = foldDoc 0 (\_ x -> 0 + x) (\_ x -> 1 + x)

testsEj1 :: Test
testsEj1 =
  test
    [ foldDocCount doc1 ~?= 0,
      foldDocCount doc2 ~?= 1,
      foldDocCount doc3 ~?= 3,
      foldDocCount doc4 ~?= 5,
      foldDocCount doc5 ~?= 6,
      foldDocCount doc6 ~?= 3,
      foldDocCountLinea doc1 ~?= 0,
      foldDocCountLinea doc2 ~?= 0,
      foldDocCountLinea doc3 ~?= 1,
      foldDocCountLinea doc4 ~?= 2,
      foldDocCountLinea doc5 ~?= 3,
      foldDocCountLinea doc6 ~?= 1
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ doc1 ~?= vacio,
      doc2 ~?= texto "ab",
      doc2 ~?= texto "a" <+> vacio <+> texto "b",
      doc3 ~?= (texto "a" <+> linea) <+> texto "b",
      doc4 ~?= texto "a" <+> linea <+> texto "bc" <+> linea <+> texto "d",
      doc5 ~?= texto "a" <+> linea <+> texto "b" <+> linea <+> linea <+> texto "cd",
      doc6 ~?= texto "abc" <+> linea <+> texto "d",
      doc6 ~?= texto "ab" <+> texto "c" <+> linea <+> texto "d"
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 doc1 ~?= vacio,
      indentar 2 doc2 ~?= texto "ab",
      indentar 1 doc2 ~?= texto "a" <+> vacio <+> texto "b",
      indentar 2 doc3 ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 4 doc3 ~?= indentar 2 (texto "a" <+> indentar 2 (linea <+> texto "b")),
      indentar 2 doc4 ~?= texto "a" <+> indentar 1 (indentar 1 (linea <+> texto "bc" <+> linea <+> texto "d")),
      indentar 314 doc4 ~?= indentar 157 (texto "a" <+> indentar 157 (linea <+> texto "bc" <+> linea <+> texto "d")),
      indentar 4 doc5 ~?= indentar 1 (texto "a" <+> indentar 3 (linea <+> texto "b" <+> linea <+> linea <+> texto "cd")),
      indentar 101 doc6 ~?= indentar 1 (texto "abc" <+> indentar 50 (indentar 50 (linea <+> texto "d")))
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar doc1 ~?= "",
      mostrar doc2 ~?= "ab",
      mostrar doc3 ~?= "a\nb",
      mostrar (indentar 2 doc3) ~?= "a\n  b",
      mostrar doc4 ~?= "a\nbc\nd",
      mostrar (indentar 4 doc4) ~?= "a\n    bc\n    d",
      mostrar doc5 ~?= "a\nb\n\ncd",
      mostrar (indentar 3 doc5) ~?= "a\n   b\n   \n   cd",
      mostrar doc6 ~?= "abc\nd",
      mostrar (indentar 5 doc6) ~?= "abc\n     d"
    ]

texto1, texto2, int1, int2, pericles, merlina, addams, familias :: PPON
texto1 = TextoPP "hola"
texto2 = TextoPP "mundo"
int1 = IntPP 1
int2 = IntPP 2
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

testsEj5 :: Test
testsEj5 =
  test
    [ pponAtomico texto1 ~?= True,
      pponAtomico texto2 ~?= True,
      pponAtomico int1 ~?= True,
      pponAtomico int2 ~?= True,
      pponAtomico pericles ~?= False,
      pponAtomico merlina ~?= False,
      pponAtomico addams ~?= False,
      pponAtomico familias ~?= False
    ]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple texto1 ~?= True,
      pponObjetoSimple texto2 ~?= True,
      pponObjetoSimple int1 ~?= True,
      pponObjetoSimple int2 ~?= True,
      pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      pponObjetoSimple familias ~?= False,
      pponObjetoSimple merlina ~?= True
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= mostrar doc1,
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (intercalar (texto ", ") [doc2, c]) ~?= "ab, c",
      mostrar (intercalar (texto ", ") [doc3, a, b, c]) ~?= "a\nb, a, b, c",
      mostrar (intercalar (texto ", ") [doc4, doc3]) ~?= "a\nbc\nd, a\nb",
      mostrar (intercalar (texto ", ") [doc5, doc2, a]) ~?= "a\nb\n\ncd, ab, a",
      mostrar (intercalar (texto ", ") [doc6, b]) ~?= "abc\nd, b",
      mostrar (intercalar (texto ", ") [indentar 1 doc2, indentar 2 doc3]) ~?= "ab, a\n  b",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",
      mostrar (entreLlaves [doc2, b, a]) ~?= "{\n  ab,\n  b,\n  a\n}",
      mostrar (entreLlaves [doc3, doc2, doc1]) ~?= "{\n  a\n  b,\n  ab,\n  \n}",
      mostrar (entreLlaves [doc4, c]) ~?= "{\n  a\n  bc\n  d,\n  c\n}",
      mostrar (entreLlaves [doc5, doc1]) ~?= "{\n  a\n  b\n  \n  cd,\n  \n}",
      mostrar (entreLlaves [doc6, a]) ~?= "{\n  abc\n  d,\n  a\n}"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (doc1 <+> a <+> doc1)) ~?= "a",
      mostrar (aplanar (doc2 <+> b)) ~?= "abb",
      mostrar (aplanar (doc3 <+> c)) ~?= "a bc",
      mostrar (aplanar (doc4 <+> a <+> c)) ~?= "a bc dac",
      mostrar (aplanar (doc5 <+> doc2)) ~?= "a b  cdab",
      mostrar (aplanar (doc6 <+> doc3)) ~?= "abc da b",
      mostrar (aplanar (indentar 2 doc3 <+> doc2)) ~?= "a bab",
      mostrar (aplanar (texto "{" <+> linea <+> a <+> linea <+> texto "}")) ~?= mostrar (aplanar (entreLlaves [a]))
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc texto1) ~?= "\"hola\"",
      mostrar (pponADoc texto2) ~?= "\"mundo\"",
      mostrar (pponADoc int1) ~?= "1",
      mostrar (pponADoc int2) ~?= "2",
      mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      mostrar (pponADoc merlina) ~?= "{ \"nombre\": \"Merlina\", \"edad\": 24 }"
    ]

import Test.HUnit
import Solucion
import Data.List
-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


testsEjesMinuscula = test [
    "Caso 0: minùscula en el medio (test catedra)" ~: (esMinuscula 'd') ~?= True,
    "Caso 1: primera minúscula" ~: (esMinuscula 'a') ~?= True,
    "Caso 2: última minúscula" ~: (esMinuscula 'z') ~?= True,
    "Caso 3: mayúscula" ~: (esMinuscula 'A') ~?= False,
    "Caso 4: caracter espacio" ~: (esMinuscula ' ') ~?= False,
    "Caso 5: otro caracter" ~: (esMinuscula '+') ~?= False
    ]

testsEjletraANatural = test [
    "Caso 0: letra del medio (test catedra)" ~: (letraANatural 'b') ~?= 1,
    "Caso 1: a" ~: (letraANatural 'a') ~?= 0,
    "Caso 2: z" ~: (letraANatural 'z') ~?= 25
    ]

testsEjdesplazar = test [
    "Caso 0: n positivo (test catedra)" ~: (desplazar 'a' 3) ~?= 'd',
    "Caso 1: n positivo más de una vuelta" ~: (desplazar 'j' 330 ) ~?= 'b',
    "Caso 2: n negativo" ~: (desplazar 'm' (-10) ) ~?= 'c',
    "Caso 3: da la vuelta" ~: (desplazar 'x' 5 ) ~?= 'c',
    "Caso 4: da la vuelta al reves" ~: (desplazar 'b' (-11) ) ~?= 'q',
    "Caso 5: n = 0" ~: (desplazar 'z' 0 ) ~?= 'z',
    "Caso 6: c no es una minúscula" ~: (desplazar '+' 0 ) ~?= '+'  
    ]

testsEjcifrar = test [
    "Caso 0: todas minúsculas (test catedra)" ~: (cifrar "computacion" 3) ~?= "frpsxwdflrq",
    "Caso 1: palabra es vacía" ~: (cifrar [] 5) ~?= [],
    "Caso 2: todas mayúsculas" ~: (cifrar "COMPUTACION" 9) ~?= "COMPUTACION",
    "Caso 3: mayúsculas y minúsculas intercaladas" ~: (cifrar "comPuTAcioN" 17) ~?= "tfdPlTAtzfN",
    "Caso 4: n = 0" ~: (cifrar "mile" 0) ~?= "mile"
    ]

testsEjdescifrar = test [
    "Caso 0: todas minúsculas (test catedra)" ~: (descifrar "frpsxwdflrq" 3) ~?= "computacion",
    "Caso 1: palabra es vacía" ~: (descifrar [] 4) ~?= [],
    "Caso 2: todas mayúsculas" ~: (descifrar "COMPUTACION" 9) ~?= "COMPUTACION",
    "Caso 3: mayúsculas y minúsculas intercaladas" ~: (descifrar "tfdPlTAtzfN" 17) ~?= "comPuTAcioN",
    "Caso 4: n = 0" ~: (descifrar "mile" 0) ~?= "mile"
    ]

testsEjcifrarLista = test [
    "Caso 0: palabras en minúscula (test catedra)" ~: (cifrarLista ["compu", "labo", "intro"]) ~?= ["compu", "mbcp", "kpvtq"],
    "Caso 1: palabras en mayúscula" ~: (cifrarLista ["COMPUTACION", "ALGORITMOS", "CODIGO"]) ~?= ["COMPUTACION", "ALGORITMOS", "CODIGO"],
    "Caso 2: algunas mayúsuclas y otras minúsculas" ~: (cifrarLista ["computacion", "algoritmos", "CODIGO", "programacion", "LABO"]) ~?= ["computacion", "bmhpsjunpt", "CODIGO", "surjudpdflrq", "LABO"],
    "Caso 3: palabras con letras mayúsculas y minúsculas" ~: (cifrarLista ["CompUtACion", "aLgorITmoS", "CODiGo"]) ~?= ["CompUtACion", "bLhpsITnpS", "CODkGq"],
    "Caso 4: lista vacía" ~: (cifrarLista []) ~?= []
    ]

testsEjfrecuencia = test [
    "Caso 0: palabra con letras repetidas (test catedra)" ~: expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
    "Caso 1: palabras es vacía" ~: (frecuencia []) ~?= [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 2: palabra en mayúscula" ~: (frecuencia "COMPUTACION") ~?= [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 3: palabra sin letras repetidas" ~: (frecuencia "intro") ~?= [0,0,0,0,0,0,0,0,20.0,0,0,0,0,20.0,20.0,0,0,20.0,0,20.0,0,0,0,0,0,0],
    "Caso 4: palabra con minúsculas y mayúsculas sin repetidas" ~: (frecuencia "IntRO") ~?= [0,0,0,0,0,0,0,0,0,0,0,0,0,50.0,0,0,0,0,0,50.0,0,0,0,0,0,0],
    "Caso 5: palabra con minúsculas y mayúsculas con minúsculas repetidas" ~: expectlistProximity (frecuencia "CodIGo") [0,0,0,33.333333,0,0,0,0,0,0,0,0,0,0,66.666667,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 6: palabra con minúsculas y mayúsculas con mayúsculas repetidas" ~: expectlistProximity (frecuencia "CodIgO") [0,0,0,33.333333,0,0,33.333333,0,0,0,0,0,0,0,33.333333,0,0,0,0,0,0,0,0,0,0,0]
 ]

testsEjcifradoMasFrecuente = test [
    "Caso 0: dos caracteres iguales (tets catedra)" ~: expectAnyTuplaAprox (cifradoMasFrecuente "taller" 3) [('o', 33.333336)], 
    "Caso 1: todos caracteres distintos" ~: expectAnyTuplaAprox (cifradoMasFrecuente "intro" 4) [('m', 20.0),('r', 20.0),('x', 20.0),('v', 20.0),('s', 20.0)],
    "Caso 2: varios pares de caracteres iguales" ~: expectAnyTuplaAprox (cifradoMasFrecuente "mayusculas" 3) [('d', 20.0),('x', 20.0),('v', 20.0)],
    "Caso 3: el caracter más repetido" ~: (cifradoMasFrecuente "cuadrada" 5) ~?= ('f', 37.5),
    "Caso 4: letras mayúsculas y minúsculas sin repetir" ~: expectAnyTuplaAprox (cifradoMasFrecuente "IntRo" 1) [('o', 33.333332),('u', 33.333332),('p', 33.333332)],
    "Caso 5: letras mayúsculas y minúsculas con minúsculas repetidas" ~: (cifradoMasFrecuente "CoDigo" 2) ~?= ('q', 50.0),
    "Caso 6: letras mayúsculas y minúsculas con mayúsuclas repetidas" ~: expectAnyTuplaAprox (cifradoMasFrecuente "cOdiGO" 1) [('d', 33.333332),('e', 33.333332),('j', 33.333332)]
    ]

testsEjesDescifrado = test [
    "Caso 0: no es un descifrado (test catedra)" ~: (esDescifrado "taller" "compu") ~?= False,
    "Caso 1: misma palabra (n=0)" ~: (esDescifrado "hola" "hola") ~?= True,
    "Caso 2: es descifrado todo minúscula" ~: (esDescifrado "hola" "szwl") ~?= True,
    "Caso 3: es descifrado minúsculas y otros" ~: (esDescifrado "Hol a++" "Hzw l++") ~?= True,
    "Caso 4: es descifrado sin minúsculas" ~: (esDescifrado "H0L A++""H0L A++") ~?= True,
    "Caso 5: no es descifrado minúsculas y otros" ~: (esDescifrado "Hol a-+" "Hzw l++") ~?= False,
    "Caso 6: palabras con distinta longitud" ~: (esDescifrado "Holaa" "Hola") ~?= False
    ]

testsEjtodosLosDescifrados = test [
    "Caso 0: hay un descifrado (test catedra)" ~: expectPermutacion (todosLosDescifrados ["compu", "frpsx", "mywza"]) [("compu", "frpsx"), ("frpsx", "compu")],
    "Caso 1: Lista vacia" ~: (todosLosDescifrados []) ~?=  [],
    "Caso 2: no hay descifrados" ~: (todosLosDescifrados ["hola","vaca", "afhs" ]) ~?= [],
    "Caso 3: hay varios descifrados" ~: expectPermutacion (todosLosDescifrados ["chau", "vaca", "inga", "hola","afhf"]) [("vaca","afhf"),("afhf","vaca"),("inga","chau"),("chau","inga")],
    "Caso 4: varios descifrados misma palabra" ~: expectPermutacion (todosLosDescifrados ["hola", "vaca", "afhf", "chau","xcec"]) [("vaca","afhf"),("afhf","vaca"),("vaca","xcec"),("xcec","vaca"), ("xcec","afhf"), ("afhf","xcec")]
    ]

testsEjexpandirClave = test [
    "Caso 0: clave mas corta que n (test catedra)" ~: (expandirClave "compu" 8) ~?= "compucom",
    "Caso 1: clave del mismo tamaño que n" ~: (expandirClave "compu" 5) ~?= "compu",
    "Caso 2: clave mas larga que n" ~: (expandirClave "compucomputadora" 5) ~?= "compu",
    "Caso 3: clave con una sola letra" ~: (expandirClave "a" 3) ~?= "aaa"
    ]

testsEjcifrarVigenere = test [
    "Caso 0: clave más corta que el mensaje (test catedra)" ~: (cifrarVigenere "computacion" "ip") ~?= "kdueciirqdv",
    "Caso 1: el resultado tiene q ser el mismo" ~: (cifrarVigenere "hola" "a") ~?= "hola",
    "Caso 2: clave más larga que el mensaje" ~: (cifrarVigenere "hola" "abcdabcd") ~?= "hpnd",
    "Caso 3: mensaje vacio" ~: (cifrarVigenere "" "clave") ~?= "",
    "Caso 4: palabra sin minúsculas" ~: (cifrarVigenere "COMPUTACION" "ip") ~?= "COMPUTACION",
    "Caso 5: palabra con mayúsculas y minúsculas" ~: (cifrarVigenere "cOMpuTaciON" "ip") ~?= "kOMecTirqON"
    ]

testsEjdescifrarVigenere = test [
    "Caso 0: clave más corta que el mensaje (test catedra)" ~: (descifrarVigenere "kdueciirqdv" "ip") ~?= "computacion",
    "Caso 1: el resultado tiene q ser el mismo" ~: (descifrarVigenere "hola" "a") ~?= "hola",
    "Caso 2: clave más larga que el mensaje" ~: (descifrarVigenere "hpnd" "abcdabcd") ~?= "hola",
    "Caso 3: mensaje vacio" ~: (descifrarVigenere "" "clave") ~?= "",
    "Caso 4: palabra sin minúsculas" ~: (descifrarVigenere "COMPUTACION" "ip") ~?= "COMPUTACION",
    "Caso 5: palabra con mayúsculas y minúsculas" ~: (descifrarVigenere "kOMecTirqON" "ip") ~?= "cOMpuTaciON"
    ]

testsEjpeorCifrado = test [
    "Caso 0: test básico (test catedra)" ~: (peorCifrado "computacion" ["ip", "asdef", "ksy"]) ~?= "asdef",
    "Caso 2: una sola clave" ~: (peorCifrado "hello" ["abc"]) ~?= "abc",
    "Caso 3: claves que resultan en cifrados igual de malos" ~: expectAny (peorCifrado "helloo" ["abc","ffff", "xyz", "bca", "cab","xx"]) ["abc", "bca", "cab"],
    "Caso 4: texto vacío" ~: expectAny (peorCifrado "" ["abc", "def"]) ["abc", "def"]
    ]

testsEjcombinacionesVigenere = test [
    "Caso 0: varios elementos, solo una dupla cumple (test catedra)" ~: (combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb") ~?= [("hola", "b")],
    "Caso 1: vacíos" ~: (combinacionesVigenere [] [] "cifrado") ~?= [],
    "Caso 2: ninguna combinación cumple" ~: (combinacionesVigenere ["hola","casa","vaca"] ["a","b","abc"] "cifrado") ~?= [],
    "Caso 3: ambas listas de un elemento" ~: (combinacionesVigenere ["hola"] ["b"] "ipmb") ~?= [("hola","b")],
    "Caso 4: combinaciones con elementos distintos cumplen" ~: expectPermutacion (combinacionesVigenere ["vaca", "hola","lata"] ["sqve","e","oene"] "zege") [("hola","sqve"),("vaca","e"),("lata","oene")],
    "Caso 5: mismo msj cumple con varias claves" ~: expectPermutacion (combinacionesVigenere ["cal","mar","solo","tanto"] ["u","uuuuu","eua"] "gul") [("mar","u"),("mar","uuuuu"),("cal","eua")],
    "Caso 6: string vacío en msj y cifrado vacío" ~: expectPermutacion (combinacionesVigenere [[], "hola","huevo"] ["clave", "key", "papel"] []) [([],"clave"), ([], "key"), ([],"papel")],
    "Caso 7: string vacío en msj y cifrado no vacío" ~: (combinacionesVigenere ["hola", []] ["clave","key"] "cifrado") ~?= []
    ]

-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)


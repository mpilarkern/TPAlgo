import Solucion
import Test.HUnit --preguntar si hace falta poner todos los resultados válidos como opcion o solo los que sabemos que da nuestra funcion

ejecutarCifrar = runTestTT testCifrar

testCifrar = test [
    "Caso 1: palabra es vacía" ~: (cifrar [] 5) ~?= [],
    "Caso 2: todas minúsculas" ~: (cifrar "computacion" 6) ~?= "iusvazgiout",
    "Caso3: todas mayúsculas" ~: (cifrar "COMPUTACION" 9) ~?= "COMPUTACION",
    "Caso 4: mayúsculas y minúsculas intercaladas" ~: (cifrar "comPuTAcioN" 17) ~?= "tfPlTAtzfN" 
    ]


ejecutarDescifrar = runTestTT testDescifrar

testDescifrar = test [
    "Caso 1: palabra es vacía" ~: (descifrar [] 4) ~?= [],
    "Caso 2: todas minúsculas" ~: (descifrar "iusvazgiout" 6) ~?= "computacion",
    "Caso 3: todas mayúsculas" ~: (descifrar "COMPUTACION" 9) ~?= "COMPUTACION",
    "Caso 4: mayúsculas y minúsculas intercaladas" ~: (descifrar "tfPlTAtzfN" 17) ~?= "comPuTAcioN"
    ]


ejecutarCifrarLista = runTestTT testCifrarLista

testCifrarLista = test [
    "Caso 1: palabras en minúscula" ~: (cifrarLista ["computacion", "algoritmos", "codigo"]) ~?= ["computacion", "bmhpsjumpt", "eqfkiq"],
    "Caso 2: palabras en mayúscula" ~: (cifrarLista ["COMPUTACION", "ALGORITMOS", "CODIGO"]) ~?= ["COMPUTACION", "ALGORITMOS", "CODIGO"],
    "Caso 3: algunas mayúsuclas y otras minúsculas" ~: (cifrarLista ["computacion", "algoritmos", "CODIGO", "programacion", "LABO"]) ~?= ["computacion", "bmhpsjumpt", "CODIGO", "surjudodflrp", "LABO"],
    "Caso 4: palabras con letras mayúsculas y minúsculas" ~: (cifrarLista ["CompUtACion", "aLgorITmoS", "CODiGo"]) ~?= ["CompUtACion", "bLhpsITnpS", "CODjGp"],
    "Caso 5: lista vacía" ~: (cifrarLista []) ~?= []
    ]

ejecutarPeorCifrado = runTestTT testPeorCifrado

testPeorCifrado = test [
    "Caso 1: básico" ~: (peorCifrado "hello" ["abc", "def", "ghi"]) ~?= "abc",
    "Caso 2: una sola clave" ~: (peorCifrado "hello" ["abc"]) ~?= "abc",
    "Caso 3: claves que resultan en cifrados idénticos" ~: (peorCifrado "hello" ["abc", "bca", "cab"]) ~?= "abc",
    "Caso 4: claves de longitud variada" ~: (peorCifrado "helloworld" ["a", "ab", "abc", "abcd"]) ~?= "a",
    "Caso 5: cifrado sin cambios" ~: (peorCifrado "aaa" ["z", "yz", "xyz"]) ~?= "z", ---ExpectAny
    "Caso 6: claves con la misma letra" ~: peorCifrado "abcde" ["aaaaa", "bbbbb", "ccccc"] ~?= "aaaaa",
    "Caso 7: texto largo con múltiples claves complejas" ~: (peorCifrado "sometextsomemoretext" ["complexkey", "simplekey", "rotatingkey"] ) ~?= "complexkey",
    "Caso 8: texto vacío" ~: (peorCifrado "" ["abc", "def"]) ~?= "abc"
    ]

ejecutarCombinacionesVigenere = runTestTT testCombinacionesVigenere

testCombinacionesVigenere = test [
    "Caso 1: vacíos" ~: (combinacionesVigenere [] [] "cifrado") ~?= [],
    "Caso 2: ninguna combinación cumple" ~: (combinacionesVigenere ["hola","casa","vaca"] ["a","b","abc"] "cifrado") ~?= [],
    "Caso 3: un elemento c/u" ~: (combinacionesVigenere ["hola"] ["b"] "ipmb") ~?= [("hola","b")],
    "Caso 4: varios elementos, solo una dupla cumple" ~: (combinacionesVigenere ["mundo","home","vaca"] ["abc","clave","f"] "mtrj") ~?= [("home","f")],
    "Caso 5: combinaciones con elementos distintos cumplen" ~: (combinacionesVigenere ["vaca", "hola","lata"] ["sqve","e","oene"] "zege") ~?= [("hola","sqve"),("vaca","e"),("lata","oene")],
    "Caso 6: mismo msj cumple con varias claves" ~: (combinacionesVigenere ["cal","mar","solo","tanto"] ["u","uuuuu","eua"] "gul") ~?= [("mar","u"),("mar","uuuuu"),("cal","eua")]
    ]

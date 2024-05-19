import Solucion
import Test.HUnit --preguntar si hace falta poner todos los resultados válidos como opcion o solo los que sabemos que da nuestra funcion


ejecutarEsMinuscula = runTestTT testEsMinuscula
testEsMinuscula = test [
    "Caso 1: primera minúscula" ~: (esMinuscula 'a') ~?= True,
    "Caso 2: otra minúscula" ~: (esMinuscula 'y') ~?= True,
    "Caso 3: última minúscula" ~: (esMinuscula 'z') ~?= True,
    "Caso 4: mayúscula" ~: (esMinuscula 'A') ~?= False,
    "Caso 5: caracter espacio" ~: (esMinuscula ' ') ~?= False,
    "Caso 5: otro caracter" ~: (esMinuscula '+') ~?= False
  ]

ejecutarLetraANatural = runTestTT testLetraANatural
testLetraANatural = test [
    "Caso 1: a" ~: (letraANatural 'a') ~?= 0,
    "Caso 2: o" ~: (letraANatural 'o') ~?= 14,
    "Caso 3: z" ~: (letraANatural 'z') ~?= 25   
  ]

ejecutarDesplazar = runTestTT testDesplazar
testDesplazar = test [
    "Caso 1: n positivo" ~: (desplazar 'a' 1 ) ~?= 'b',
    "Caso 2: n positivo más de una vuelta" ~: (desplazar 'j' 330 ) ~?= 'b',
    "Caso 3: n negativo" ~: (desplazar 'm' (-10) ) ~?= 'c',
    "Caso 4: da la vuelta" ~: (desplazar 'x' 5 ) ~?= 'c', 
    "Caso 5: da la vuelta al reves" ~: (desplazar 'b' (-11) ) ~?= 'q', 
    "Caso 6: n = 0" ~: (desplazar 'z' 0 ) ~?= 'z',
    "Caso 7: c no es una minúscula" ~: (desplazar '+' 0 ) ~?= '+'   
 ]

ejecutarCifrar = runTestTT testCifrar

testCifrar = test [
    "Caso 1: palabra es vacía" ~: (cifrar [] 5) ~?= [],
    "Caso 2: todas minúsculas" ~: (cifrar "computacion" 6) ~?= "iusvazgiout",
    "Caso 3: todas mayúsculas" ~: (cifrar "COMPUTACION" 9) ~?= "COMPUTACION",
    "Caso 4: mayúsculas y minúsculas intercaladas" ~: (cifrar "comPuTAcioN" 17) ~?= "tfdPlTAtzfN", 
    "Caso 5: n = 0" ~: (cifrar "mile" 0) ~?= "mile"
     ]


ejecutarDescifrar = runTestTT testDescifrar

testDescifrar = test [
    "Caso 1: palabra es vacía" ~: (descifrar [] 4) ~?= [],
    "Caso 2: todas minúsculas" ~: (descifrar "iusvazgiout" 6) ~?= "computacion",
    "Caso 3: todas mayúsculas" ~: (descifrar "COMPUTACION" 9) ~?= "COMPUTACION",
    "Caso 4: mayúsculas y minúsculas intercaladas" ~: (descifrar "tfdPlTAtzfN" 17) ~?= "comPuTAcioN",
    "Caso 5: n = 0" ~: (descifrar "mile" 0) ~?= "mile"
    ]


ejecutarCifrarLista :: IO Counts
ejecutarCifrarLista = runTestTT testCifrarLista

testCifrarLista :: Test
testCifrarLista = test [
    "Caso 1: palabras en minúscula" ~: (cifrarLista ["computacion", "algoritmos", "codigo"]) ~?= ["computacion", "bmhpsjumpt", "eqfkiq"],
    "Caso 2: palabras en mayúscula" ~: (cifrarLista ["COMPUTACION", "ALGORITMOS", "CODIGO"]) ~?= ["COMPUTACION", "ALGORITMOS", "CODIGO"],
    "Caso 3: algunas mayúsuclas y otras minúsculas" ~: (cifrarLista ["computacion", "algoritmos", "CODIGO", "programacion", "LABO"]) ~?= ["computacion", "bmhpsjumpt", "CODIGO", "surjudodflrp", "LABO"],
    "Caso 4: palabras con letras mayúsculas y minúsculas" ~: (cifrarLista ["CompUtACion", "aLgorITmoS", "CODiGo"]) ~?= ["CompUtACion", "bLhpsITnpS", "CODjGp"],
    "Caso 5: lista vacía" ~: (cifrarLista []) ~?= []
    ]

ejecutarTestFrecuencia = runTestTT testFrecuencia
testFrecuencia :: Test
testFrecuencia = test [
    "Caso 1: palabras es vacía" ~: (frecuencia []) ~?= [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 2: palabra en mayúscula" ~: (frecuencia "COMPUTACION") ~?= [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 3: palabra sin letras repetidas" ~: (frecuencia "intro") ~?= [0,0,0,0,0,0,0,0,20.0,0,0,0,0,20.0,20.0,0,0,20.0,0,20.0,0,0,0,0,0,0],
    "Caso 4: palabra con letras repetidas" ~: (frecuencia "codigo") ~?= [0,0,16.666666,16.666666,0,0,16.666666,0,16.666666,0,0,0,0,0,33.333332,0,0,0,0,0,0,0,0,0,0,0], --preguntar lo de igualdad de floats
    "Caso 5: palabra con minúsculas y mayúsculas sin repetidas" ~: (frecuencia "IntRO") ~?= [0,0,0,0,0,0,0,0,0,0,0,0,0,50.0,0,0,0,0,0,50.0,0,0,0,0,0,0],
    "Caso 6: palabra con minúsculas y mayúsculas con minúsculas repetidas" ~: (frecuencia "CodIGo") ~?= [0,0,0,33.333332,0,0,0,0,0,0,0,0,0,0,66.666667,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 7: palabra con minúsculas y mayúsculas con mayúsculas repetidas" ~: (frecuencia "CodIgO") ~?= [0,0,0,33.333332,0,0,33.333332,0,0,0,0,0,0,0,33.333332,0,0,0,0,0,0,0,0,0,0,0]
 ]


ejecutarEsDescifrado = runTestTT testEsDescifrado
testEsDescifrado = test [
    "Caso 1: no es un descifrado" ~: esDescifrado ("hola","holu") ~?= False,
    "Caso 2: misma palabra (n=0)" ~: esDescifrado ("hola","hola") ~?= True,
    "Caso 3: es descifrado todo minúscula" ~: esDescifrado ("hola","szwl") ~?= True,
    "Caso 4: es descifrado minúsculas y otros" ~: esDescifrado ("Hol a++","Hzw l++") ~?= True,
    "Caso 5: es descifrado sin minúsculas" ~: esDescifrado ("H0L A++","H0L A++") ~?= True,
    "Caso 6: no es descifrado minúsculas y otros" ~: esDescifrado ("Hol a-+","Hzw l++") ~?= False,
    "Caso 7: palabras con distinta longitud" ~: esDescifrado ("Holaa","Hola") ~?= False
  ]


ejecutarExpandirClave = runTestTT testExpandirClave

testExpandirClave = test [
    "Caso 1: clave mas corta que n" ~: expandirClave "compu" 10 ~?= "compucompu",
    "Caso 2: clave del mismo tamaño que n" ~: expandirClave "compu" 5 ~?= "compu",
    "Caso 3: clave mas larga que n" ~: expandirClave "compucomputadora" 5 ~?= "compu",
    "Caso 4: clave con una sola letra" ~: expandirClave "a" 3 ~?= "aaa"
    ]

ejecutarCifrarVigenere = runTestTT testCifrarVigenere

testCifrarVigenere = test [
    "Caso 1: el resultado tiene q ser el mismo" ~: cifrarVigenere "hola" "a" ~?= "hola",
    "Caso 2: clave más larga que el mensaje" ~: cifrarVigenere "hola" "abcdabcd" ~?= "hola",
    "Caso 3: mensaje vacio" ~: cifrarVigenere "" "clave" ~?= "",
    "Caso 4: cifrado que rota toda la palabra" ~: cifrarVigenere "hola" "zzzz" ~?= "hnkz",
    "Caso 5: test del TP" ~: cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv"
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

import Solucion
import Test.HUnit --preguntar si hace falta poner todos los resultados válidos como opcion o solo los que sabemos que da nuestra funcion


ejecutarEsMinusucula = runTestTT testEsMinusucula
testEsMinusucula = test [
    "caso1" ~: (esMinuscula 'a') =?~ True,
    "caso2" ~: (esMinuscula 'b') =?~ True,
    "caso3" ~: (esMinuscula 'c') =?~ True,
    "caso4" ~: (esMinuscula 'd') =?~ True,
    "caso5" ~: (esMinuscula 'e') =?~ True,
    "caso6" ~: (esMinuscula 'A') =?~ False,
    "caso7" ~: (esMinuscula 'B') =?~ False,
    "caso8" ~: (esMinuscula 'C') =?~ False,
    "caso9" ~: (esMinuscula 'D') =?~ False,
    "caso10" ~: (esMinuscula 'E') =?~ False
 ]

ejecutarLetraANatural = runTestTT testLetraANatural
testLetraANatural = test [
    "caso a" ~: (letraANatural 'a') =?~ 0,
    "caso b" ~: (letraANatural 'b') =?~ 1,
    "caso c" ~: (letraANatural 'c') =?~ 2,
    "caso d" ~: (letraANatural 'd') =?~ 3,
    "caso e" ~: (letraANatural 'e') =?~ 4,
    "caso f" ~: (letraANatural 'f') =?~ 5,
    "caso g" ~: (letraANatural 'g') =?~ 6,
    "caso h" ~: (letraANatural 'h') =?~ 7,
    "caso i" ~: (letraANatural 'i') =?~ 8,
    "caso j" ~: (letraANatural 'j') =?~ 9,
    "caso k" ~: (letraANatural 'k') =?~ 10,
    "caso l" ~: (letraANatural 'l') =?~ 11,
    "caso m" ~: (letraANatural 'm') =?~ 12,
    "caso n" ~: (letraANatural 'n') =?~ 13,
    "caso o" ~: (letraANatural 'o') =?~ 14,
    "caso p" ~: (letraANatural 'p') =?~ 15,
    "caso q" ~: (letraANatural 'q') =?~ 16,
    "caso r" ~: (letraANatural 'r') =?~ 17,
    "caso s" ~: (letraANatural 's') =?~ 18,
    "caso t" ~: (letraANatural 't') =?~ 19,
    "caso u" ~: (letraANatural 'u') =?~ 20,
    "caso v" ~: (letraANatural 'v') =?~ 21,
    "caso w" ~: (letraANatural 'w') =?~ 22,
    "caso x" ~: (letraANatural 'x') =?~ 23,
    "caso y" ~: (letraANatural 'y') =?~ 24,
    "caso z" ~: (letraANatural 'z') =?~ 25
 ]

ejecutarDesplazar = runTestTT testDesplazar
testDesplazar = test [
    "caso: desplaza 1" ~: (desplazar 'a' 1 ) =?~ 'b',
    "caso: desplaza 5" ~: (desplazar 'a' 5 ) =?~ 'f',
    "caso: vuelve a empezar" ~: (desplazar 'z' 1 ) =?~ 'a',
    "caso: desplaza un numero grande" ~: (desplazar 'j' 330 ) =?~ 'b',
    "caso: desplaza uno negativo" ~: (desplazar 'm' (-10) ) =?~ 'c',
    "caso: da la vuelta" ~: (desplazar 'x' 5 ) =?~ 'c', 
    "caso: da la vuelta al reves" ~: (desplazar 'b' (-11) ) =?~ 'q', 
    "caso: desplaza un negativo grande" ~: (desplazar 'd' (-157) ) =?~ 'c',
    "caso: no se desplaza" ~: (desplazar 'z' 0 ) =?~ 'z'
]

ejecutarCifrar = runTestTT testCifrar
testCifrar = test [ 
    "caso: cifra -1" ~: (cifrar "vaca" (-1)) =?~ "uzbz",
    "caso" ~: (cifrar "vaca" 2) =?~ "xcec",
    "caso" ~: (cifrar "mile" 3) =?~ "ploh",
    "caso" ~: (cifrar "mile" 148) =?~ "eadw",
    "caso: cifra un n negativo grande" ~: (cifrar "pilar" (-180)) =?~ "rknct",
    "caso: cifra el resultado del anterior" ~: (cifrar "rknct" 180) =?~ "pilar",
    "caso: no cifra " ~: (cifrar "ivan" 0) =?~ "ivan",
    "caso: " ~: (cifrar "arbol" (-8)) =?~ "sjtgd",
    "caso" ~: (cifrar "vicky" (-5)) =?~ "qdxft"
]

ejecutarDescifrar = runTestTT testDescifrar
testDescifrar = test [
    "caso" ~: (descifrar "uzbz" (-1)) =?~ "vaca",
    "caso" ~: (descifrar "vaca" 1) =?~ "uzbz",
    "caso" ~: (descifrar "vaca" 27) =?~ "uzbz",
    "caso" ~: (descifrar "eadw" 148) =?~ "mile",
    "caso" ~: (descifrar "vaca" 27) =?~ "uzbz"
    ,
]

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

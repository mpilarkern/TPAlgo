import Solucion
import Test.HUnit

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

ejecutarFrecuencia = runTestTT testFrecuencia

testFrecuencia = test [
    "Caso 1: palabras es vacía" ~: (frecuencia []) ~?= [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 2: palabra en mayúscula" ~: (frecuencia "COMPUTACION") ~?= [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 3: palabra sin letras repetidas" ~: (frecuencia "intro") ~?= [0,0,0,0,0,0,0,0,20.0,0,0,0,0,20.0,20.0,0,0,20.0,0,20.0,0,0,0,0,0,0],
    "Caso 4: palabra con letras repetidas" ~: (frecuencia "codigo") ~?= [0,0,16.666668,16.666668,0,0,16.666668,0,16.666668,0,0,0,0,0,33.333334,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 5: palabra con minúsculas y mayúsculas sin repetidas" ~: (frecuencia "IntRO") ~?= [0,0,0,0,0,0,0,0,0,0,0,0,0,50.0,0,0,0,0,0,50.0,0,0,0,0,0,0],
    "Caso 6: palabra con minúsculas y mayúsculas con minúsculas repetidas" ~: (frecuencia "CodIGo") ~?= [0,0,0,33.333334,0,0,0,0,0,0,0,0,0,0,66.666667,0,0,0,0,0,0,0,0,0,0,0],
    "Caso 7: palabra con minúsculas y mayúsculas con mayúsculas repetidas" ~: (frecuencia "CodIgO") ~?= [0,0,0,33.333334,0,0,33.333334,0,0,0,0,0,0,0,33.333334,0,0,0,0,0,0,0,0,0,0,0]
 ]

ejecutarCifradoMasFrecuente = runTestTT testCifradoMasFrecuente

testCifradoMasFrecuente = test [
    "Caso 1: todos caracteres distintos" ~: expectAny (cifradoMaFrecuente "intro" 4) [('m', 20.0),('r', 20.0),('x', 20.0),('v', 20.0),('s', 20.0)],
    "Caso 2: dos caracteres iguales" ~: (cifradoMaFrecuente "codigo" 2) ~?= ('q', 33.333332),
    "Caso 3: varios pares de caracteres iguales" ~: expectAny (cifradoMaFrecuente "mayusculas" 3) [('d', 20.0),('x', 20.0),('v', 20.0)],
    "Caso 4: el caracter más repetido" ~: (cifradoMaFrecuente "cuadrada" 5) ~?= ('f', 37.5),
    "Caso 5: letras mayúsculas y minúsculas sin repetir" ~: expectAny (cifradoMaFrecuente "IntRo" 1) [('o', 33.333332),('u', 33.333332),('p', 33.333332)],
    "Caso 6: letras mayúsculas y minúsculas con minúsculas repetidas" ~: (cifradoMaFrecuente "CoDigo" 2) ~?= ('q', 50.0),
    "Caso 5: letras mayúsculas y minúsculas con mayúsuclas repetidas" ~: expectAny (cifradoMaFrecuente "cOdiGO" 1) [('d', 33.333332),('e', 33.333332),('j', 33.333332)],
 ]

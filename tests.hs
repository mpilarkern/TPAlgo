import Solucion
import Test.HUnit

import Data.List

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


 ejecutarCifrarLista = runTestTT testCifrarLista

 testCifrarLista = test [
    "Caso 1: palabras en minúscula" ~: (cifrarLista ["computacion", "algoritmos", "codigo"]) ~?= ["computacion", "bmhpsjumpt", "eqfkiq"],
    "Caso 2: palabras en mayúscula" ~: (cifrarLista ["COMPUTACION", "ALGORITMOS", "CODIGO"]) ~?= ["COMPUTACION", "ALGORITMOS", "CODIGO"],
    "Caso 3: algunas mayúsuclas y otras minúsculas" ~: (cifrarLista ["computacion", "algoritmos", "CODIGO", "programacion", "LABO"]) ~?= ["computacion", "bmhpsjumpt", "CODIGO", "surjudodflrp", "LABO"],
    "Caso 4: palabras con letras mayúsculas y minúsculas" ~: (cifrarLista ["CompUtACion", "aLgorITmoS", "CODiGo"]) ~?= ["CompUtACion", "bLhpsITnpS", "CODjGp"],
    "Caso 5: lista vacía" ~: (cifrarLista []) ~?= []
 ]
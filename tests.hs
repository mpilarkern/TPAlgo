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
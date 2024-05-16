import Solucion
import Test.HUnit

ejecutarCifrar = runTestTT testCifrar

testCifrar = test [
    "Caso 1: palabra es vacía" ~: (cifrar [] 5) ~?= []
    "Caso 2: todas minúsculas" ~: (cifrar "computacion" 6) ~?= "iusvazgiout"
    "Caso3: todas mayúsculas" ~: (cifrar "COMPUTACION" 9) ~?= "COMPUTACION"
    "Caso 4: mayúsculas y minúsculas intercaladas" ~: (cifrar "comPuTAcioN" 17) ~?= "tfPlTAtzfN" 
 ]


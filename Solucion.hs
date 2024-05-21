
module Solucion where
import Data.Char

-- Nombre de Grupo: Sambuchito
--Integrantes:
--Integrante1: 45431079, Amaro Milena Paula
--Integrante2: 46027355, Kern María Pilar 
--Integrante3: 45679153, Gorgone Victoria Abril
--Integrante4: 45749818, Schuster Iván
-- Integrantes que abandonaron la materia: ()

--EJ 1: Devuelve True si el caracter 'c' tiene orden entre 97 y 122 del código ASCII (las minúsculas) y False en cualquier otro caso.
esMinuscula :: Char -> Bool   
esMinuscula c | 97 <= ord c && ord c <= 122 = True
              | otherwise = False

--EJ 2: Dada una 'c' minúscula, le resta 97 a su orden según el código ASCII y así devuelve su posición en el abecedario.
letraANatural:: Char -> Int 
letraANatural c = ord c - 97

--EJ 3: 

--Si 'c' no es minúscula, devuelve 'c'. 
--Si 'c' es minúscula y sumándole n al código ASCII de 'c' este número se mantiene entre 97 y 122 (sigue siendo el ASCII de una minúscula), devuelve la minúscula con ese código.
--En caso de que ese número sea menor a 97 o se pase de 122, vuelve a realizar la operción pero sumando o restando 26 (vuelta completa al abecedario) y así hasta que n + ord c quede dentro del rango del código de las minúsculas.
desplazar:: Char -> Int -> Char 
desplazar c n | not (esMinuscula c) = c
              | 97 <= (ord c + n) && (ord c + n) <= 122 = chr (ord c + n)
              | (ord c + n) < 97 = desplazar c (n + 26)
              | (ord c + n) > 122 = desplazar c (n - 26)
--EJ 4:
--Si la palabra es un String vacío, devuelve String vacío.
--Sino toma la primera letra de la palabra y la desplaza n posiciones en el alfabeto.
--Luego desplaza la siguiente letra n posiciones y así hasta la última letra. Devuelve toda la palabra desplazada n posiciones.
cifrar:: String -> Int -> String
cifrar palabra n |  null palabra = []
                 | otherwise = desplazar (head palabra) n : cifrar (tail palabra) n

--EJ 5:

--Usa la función "cifrar" pero con el n negativo así desplaza cada letra de la palabra n lugares hacia atrás.
descifrar:: String -> Int -> String
descifrar palabra n = cifrar palabra ((-1)*n)

--EJ 6:

-- Posición: devuelve la posición de un elemento en una lista (requiere que el elemento pertenezca a la lista).
posicion:: (Eq t) => t -> [t] -> Int  
posicion x (y:ys) | x == y = 0
                  | otherwise = 1 + posicion x ys

-- CifrarListaAux: Recibe como parámetros la misma lista de palabras dos veces. La primera lista es la que vamos "cortando" y la segunda queda siempre intacta para chequear la posición de la palabra. 
--                Toma la primera palabra de la primera lista, busca su posición en la segunda lista y cifra esta palabra con su posición como cantidad de posiciones a desplazar.
--                Luego toma la segunda palabra de la primera lista y hace lo mismo. Repite el proceso hasta el final de la primera lista y devuelve la lista de todas las palabras ya cifradas
cifrarListaAux:: [String] -> [String] -> [String] 
cifrarListaAux [] palabras = []
cifrarListaAux (p1:lp) palabras = cifrar p1 (posicion p1 palabras) : cifrarListaAux lp palabras

--Usa la función "CifrarListaAux" dándole como input ambas veces el parámetro "palabras" (la lista de palabras a cifrar). Devuelve la lista de palabras cifradas con su posición en la lista.
cifrarLista:: [String] -> [String]
cifrarLista [] = []
cifrarLista palabras = cifrarListaAux palabras palabras

--EJ 7

-- CuantasApariciones: Cuenta la cantidad de apariciones de un elemento en una lista
--                     Si el elemento no pertence a la lista, tiene 0 apariciones.
--                     Si pertenece a la lista, se fija si es igual al primer elemento de la lista. En ese caso, suma uno al contador y pasa a fijarse cuantas veces aparece en el resto de la lista
--                     Si pertenece a la lista pero no es igual al primer elemento, directamente pasa a fijarse cuantas veces aparece en la cola de la lista.
cuantasApariciones:: (Eq t) => t -> [t] -> Int
cuantasApariciones x [] = 0
cuantasApariciones x (y:ys) | not (elem x (y:ys)) = 0
                            | x == y = 1 + cuantasApariciones x ys
                            | otherwise = cuantasApariciones x ys

-- CantMinusculas: Devuelve la cantidad de minúsculas en un String
--                 Se fija si la primera letra es minúscula con "EsMinuscula", si lo es, suma uno al contador y se fija si la siguiente es minúscula. Si no es minúscula, se fija si la siguiente lo es sin sumar nada al contador.
--                 Reptite el proceso hasta el final de la palabra y devuelve la cantidad de minúsculas hallada.
cantMinusculas :: String -> Int 
cantMinusculas [] = 0
cantMinusculas (c:cs) | esMinuscula c = 1 + cantMinusculas cs
                      | otherwise = cantMinusculas cs

-- PorcentajeDeApariciones: Dado un elemento y una lista, si el elemento no pertenece a la lista, devuelve 0;
--                          si el elemento pertenece a la lista, usa "CuantasApariciones" y "CantMinusculas" para calcular el porcentaje de apariciones del elemento en la lista sobre el total de minúsculas en la lista.
porcentajeDeApariciones:: Char -> String -> Float
porcentajeDeApariciones c palabra | not (elem c palabra) = 0
                                  | otherwise = 100 * fromIntegral (cuantasApariciones c palabra) / fromIntegral ( cantMinusculas palabra)

-- FrecuenciaAux: Dada una palabra y una lista de caracteres (llamada "minusculas"), usa "PorcentajeDeApariciones" para calcular el porcentaje de apariciones de cada caracater de "minusculas" en la palabra. 
--                Va agregando los porcentajes a la lista resultado.
frecuenciaAux :: String -> String -> [Float] 
frecuenciaAux _ [] = []
frecuenciaAux palabra minusculas = porcentajeDeApariciones (head minusculas) palabra : frecuenciaAux palabra (tail minusculas)

-- Si la palabra no contiene minúsculas, devuelve la lista de 0s. Si contiene alguna minúscula, usa la función frecuenciaAux con la lista del abecedario para que calcule el porcentaje de cada letra minuscula en el abecedario sobre "palabra".
frecuencia:: String -> [Float]
frecuencia palabra | cantMinusculas palabra == 0 = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
                   | otherwise = frecuenciaAux palabra "abcdefghijklmnopqrstuvwxyz"

--EJ 8
--MaximoLista: Dada una lista de Floats devuelve aquel que sea mayor o igual a todos los demás
maximoLista:: [Float] -> Float
maximoLista [x] = x
maximoLista [x,y] | x >= y = x
                  | x < y = y
maximoLista (x:y:xs)| x >= y = maximoLista (x:xs)
                    | x < y = maximoLista (y:xs)

-- recibe una palabra y un numero y devuelve un vector, que en la primer cordenadada da el chr del maximo de la lista frecuencia palabra (suma el numero y 97 para que me de el caracter buscado en minuscula) y en la segunda coordenada el maximo de la lista frecuencia palabra, ya que ese es el porcentaje del chr encontrado.
cifradoMasFrecuente:: String -> Int -> (Char, Float) 
cifradoMasFrecuente palabra n = (chr ((posicion frecuenciaMax frecuencias) + n + 97) , frecuenciaMax)
        where 
            frecuencias = frecuencia palabra
            frecuenciaMax = maximoLista frecuencias

--EJ 9
--si ambas strings son vacías, devuelve true. Si las palabras son de distinta longitud devuelve false, pues es imposible que una sea descifrado de la otra.
--Si la primer letra de la primer palabra y la primer letra de la segunda palabra son iguales y mayúsculas, entoncesla función ve si el resto de las palabras son descifrados una de la otra.
--Si la primer letra de la primer palabra es mayúscula y es distinta ala primer letra de la segunda palabra, devuelve false.
--Si  la segunda palabra es igual al resultado de cifrar la primera palabra con n el resultado de restar el orden de la primer letra de la primer palabra y el de la segunda palabra, entonces devuelve true.
--En otro caso, devuelve false.
esDescifrado :: String -> String -> Bool
esDescifrado [] [] = True
esDescifrado (p1:pa) (p2:pb) | length (p1:pa) /= length (p2:pb) = False
                             | not (esMinuscula p1) && p1 == p2 = esDescifrado pa pb
                             | not (esMinuscula p1) && p1 /= p2 = False
                             | cifrar (p1:pa) (ord p2 - ord p1) == (p2:pb) = True 
                             | otherwise = False


-- EJ 10
--devuelve vectores de p y sus cifrados de ambas formas ([(p, cifrado),(cifrado,p)]).
--si la lista de strings es vacía, entonces devuelve una lista vacía. Sino ve si el string p es descifrado del primer elemento de la lista (p1:ps).
--Si p es descifrado de p1, entonces p1 es descifrado de p. Por lo tanto, agrega ambas duplas a la lista. Luego, hace lo mismo con el resto de elementos de la lista.
--Si p no es descifrado de p1, ve si p es descifrado de del primer elemento de ps y vuelve a empezar.
cifradosDe:: String -> [String] -> [(String, String)]  
cifradosDe p [] = []
cifradosDe p (p1:ps) | esDescifrado p p1 = (p, p1) : (p1,p) : cifradosDe p ps
                     | otherwise = cifradosDe p ps

--si la lista de strings es vacía, devuelve una lista vacía.
--Sino busca los cifrados del primer elemento en el resto de la lista con la función cifradosDe,
--luego busca los cifrados del segundo elemento en el resto de la lista y hace lo mismo con los demás elementos mientras va concatenando los resultados en una única lista.
todosLosDescifrados :: [String] -> [(String, String)] 
todosLosDescifrados [] = []
todosLosDescifrados (p1:ps) = cifradosDe p1 ps ++ todosLosDescifrados ps 

--EJ 11
--recibe como parámetro dos veces la misma clave y un entero que es la cantidad de lugares que se va a expandir dicha clave.
--El primer string es el que vamos a ir cortando y el segundo se mantiene intacto. Agrega la primera letra de la clave al string res y luego repite el proceso restandole 1 a n.
--Si la clave se queda sin caracteres antes de que n llegue a 0, vuelve a empezar el proceso con la clave completa y el n actual
expandir :: String -> Int -> String -> String
expandir _ 0 _ = []  -- si n es 0 devuelve una cadena vacia.
expandir [] n original = expandir original n original  -- reinicia la clave cuando se acaben los caracteres.
expandir (p:ps) n original = p : expandir ps (n - 1) original  -- agrega el p actual y sigue con alabra

--usa la funcion expandir dandole como input ambas veces el parámetro clave y devuelve la clave expandida.
expandirClave :: String -> Int -> String
expandirClave clave n = expandir clave n clave

-- EJ 12

-- CifrarConClaveExpandida: Recibe como parámetros una palabra y una clave ya expandida a la longitud de la palabra
--                          Calcula la posición en el abecedario de la primera letra de "claveExp", la llama n.
--                          Desplaza la primera letra de la palabra n posiciones y la agrega al String respuesta.
--                          Repite estos dos pasos con la siguiente letra de "claveExp" y de palabra. Continua el proceso hasta que tanto la palabra como "claveExp" se queden sin letras y devuelve el String respuesta.
cifrarConClaveExpandida:: String -> String -> String
cifrarConClaveExpandida [] [] = []
cifrarConClaveExpandida (s:sl) claveExp = desplazar s n : cifrarConClaveExpandida sl (tail claveExp)
            where   n = letraANatural (head claveExp)

--Recibe una palabra y una clave, expande la clave con "expandirClave" a la longitud de la palabra y le pasa la palabra y la clave expandida a "cifrarConClaveExpandida". 
cifrarVigenere :: String -> String -> String
cifrarVigenere palabra clave = cifrarConClaveExpandida palabra claveExp
        where claveExp =  expandirClave clave (length palabra)

-- EJ 13  
-- DescifrarConClaveExpandida: Recibe como parámetros una palabra y una clave ya expandida a la longitud de la palabra
--                             Calcula la posición en el abecedario de la primera letra de "claveExp", la llama n.
--                             Desplaza la primera letra de la palabra n posiciones hacia atrás y la agrega al String respuesta.
--                             Repite estos dos pasos con la siguiente letra de "claveExp" y de palabra. Continua el proceso hasta que tanto la palabra como "claveExp" se queden sin letras y devuelve el String respuesta.
descifrarConClaveExpandida:: String -> String -> String
descifrarConClaveExpandida [] [] = []
descifrarConClaveExpandida (s:sl) claveExp = desplazar s ((-1) * n) : descifrarConClaveExpandida sl (tail claveExp)
            where   n = letraANatural (head claveExp)

--Recibe una palabra y una clave, expande la clave con "expandirClave" a la longitud de la palabra y le pasa la palabra y la clave expandida a "descifrarConClaveExpandida".
descifrarVigenere :: String -> String -> String
descifrarVigenere palabra clave = descifrarConClaveExpandida palabra claveExp
        where claveExp =  expandirClave clave (length palabra)

-- EJ 14

-- Absoluto: dado un entero, devuelve su valor absoluto
absoluto :: Int -> Int
absoluto x
  | x < 0     = -x
  | otherwise = x

-- Distancia: Recibe dos palabras de igual longitud. 
--            Toma la primera letra de cada una y calcula el valor absoluto de la diferencia entre sus posiciones en el abecedario.
--            Repite lo mismo para la segunda letra de cada palabra y va sumando estos valores hasta la última letra de cada palabra. 
--            Devuelve le resultado de esa suma.
distancia :: String -> String -> Int
distancia [] [] = 0
distancia [x] [y] = absoluto (letraANatural x - letraANatural y)
distancia (x:xs) (y:ys) = absoluto (letraANatural x - letraANatural y) + distancia xs ys  

-- Recibe una palabra y una lista de claves.
-- Cifra la palabra con la primera clave y con la segunda clave de la lista. 
-- Calcula la distancia entre la palabra y cada uno de estos cifrados calculados.
-- Se queda con aquella clave que dé el menor valor de distancia entre la palabra y su cifrado. 
-- Repite el proceso para comparar esta clave con la siguiente en la lista.
-- Devuelve la clave que produjo el menor valor de distancia entre su cifrado y la palabra original, de entre todas las claves de la lista.
peorCifrado :: String -> [String] -> String 
peorCifrado palabra [clave] = clave
peorCifrado p [c1,c2] | distancia p (cifrarVigenere p c1) <= distancia p (cifrarVigenere p c2)  = c1
                      | distancia p (cifrarVigenere p c1 ) > distancia p (cifrarVigenere p c2) = c2
peorCifrado p (c1:c2:cs) | distancia p (cifrarVigenere p c1) <= distancia p (cifrarVigenere p c2)  = peorCifrado p (c1:cs)
                         | distancia p (cifrarVigenere p c1) > distancia p (cifrarVigenere p c2) = peorCifrado p (c2:cs)

--EJ 15

-- CombinacionesVigenereConUnaClave: Recibe una lista de mensajes, una clave y un String "cifrado". 
--                                   Se fija si cifrar el primer mensaje de la lista con la clave da como resultado el String "cifrado". 
--                                   Si es así, agrega el vector (msj,clave) a la lista resultado y se fija si ocurre esto mismo con el segundo mensaje de la lista.
--                                   Si no da como resultado "cifrado", directamente repite el proceso con el siguiente mensaje en la lista. Y hace esto hasta el final de la lista de mensajes.
combinacionesVigenereConUnaClave::[String] -> String -> String -> [(String, String)] 
combinacionesVigenereConUnaClave [] _ _ = []
combinacionesVigenereConUnaClave (m:ms) clave cifrado | cifrarVigenere m clave == cifrado = (m, clave) : combinacionesVigenereConUnaClave ms clave cifrado
                                                      | otherwise = combinacionesVigenereConUnaClave ms clave cifrado

-- Usa "combinacionesVigenereConUnaClave" pasándole como parámetros la lista de mensajes, la primera clave de la lista de claves y "cifrado". Así calcula todos los vectores que tienen a la primera clave en su segunda coordenada.
-- Luego hace lo mismo pero con la segunda clave de la lista de claves en vez de la primera, luego la tercera y así hasta llegar al final de la lista de claves.
-- Devuelve la concatenación de todas las listas de vectores genereadas al aplicar "combinacionesVigenereConUnaClave" a cada clave de la lista. 
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)] 
combinacionesVigenere _ [] _ = []
combinacionesVigenere [mensaje] [clave] cifrado | cifrarVigenere mensaje clave == cifrado = [(mensaje, clave)]
                                                | otherwise =  []
combinacionesVigenere mensajes (c:cs) cifrado = combinacionesVigenereConUnaClave mensajes c cifrado ++ combinacionesVigenere mensajes cs cifrado

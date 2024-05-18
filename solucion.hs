module Solucion where
import Data.Char

esMinuscula :: Char -> Bool    --Ej 1
esMinuscula c | 97 <= ord c && ord c <= 122 = True
              | otherwise = False

letraANatural:: Char -> Int --Ej 2
letraANatural c = ord c - 97

desplazar:: Char -> Int -> Char --Ej
desplazar c n | not (esMinuscula c) = c
              | 97 <= (ord c + n) && (ord c + n) <= 122 = chr (ord c + n)
              | (ord c + n) < 97 = desplazar c (n + 26)
              | (ord c + n) > 122 = desplazar c (n - 26)
            
cifrar:: String -> Int -> String
cifrar palabra n |  null palabra = []
                 | otherwise = desplazar (head palabra) n : cifrar (tail palabra) n

descifrar:: String -> Int -> String
descifrar palabra n = cifrar palabra ((-1)*n)

posicion:: (Eq t) => t -> [t] -> Int  --devuelve la posición de un elemento en una lista (requiere que el elemento pertenezca a la lista)
posicion x (y:ys) | x == y = 0
                  | otherwise = 1 + posicion x ys

cifrarListaAux:: [String] -> [String] -> [String] --preguntar al profe
cifrarListaAux [] palabras = []
cifrarListaAux (p1:lp) palabras = cifrar p1 (posicion p1 palabras) : cifrarListaAux lp palabras

cifrarLista:: [String] -> [String]
cifrarLista [] = []
cifrarLista palabras = cifrarListaAux palabras palabras

cuantasApariciones:: (Eq t) => t -> [t] -> Int
cuantasApariciones x [] = 0
cuantasApariciones x (y:ys) | not (elem x (y:ys)) = 0
                            | x == y = 1 + cuantasApariciones x ys
                            | otherwise = cuantasApariciones x ys

porcentajeDeApariciones:: Char -> String -> Float
porcentajeDeApariciones c palabra = 100 * fromIntegral (cuantasApariciones c palabra) / fromIntegral ( length palabra)

frecuenciaAux :: String -> String -> [Float] --preguntar al profe
frecuenciaAux _ [] = []
frecuenciaAux palabra minusculas = porcentajeDeApariciones (head minusculas) palabra : frecuenciaAux palabra (tail minusculas)

frecuencia:: String -> [Float]
frecuencia palabra = frecuenciaAux palabra "abcdefghijklmnopqrstuvwxyz"

maximoLista:: [Float] -> Float
maximoLista [x] = x
maximoLista [x,y] | x >= y = x
                  | x < y = y
maximoLista (x:y:xs)| x >= y = maximoLista (x:xs)
                    | x < y = maximoLista (y:xs)

cifradoMasFrecuente:: String -> Int -> (Char, Float) --usa la posición de la frecuencia máxima en la lista de frecuencias para identificar el caracter
cifradoMasFrecuente palabra n = (chr (posicion frecuenciaMax frecuencias + n + 97) , frecuenciaMax)
        where 
            frecuencias = frecuencia palabra
            frecuenciaMax = maximoLista frecuencias

nDesplazamiento :: Char -> Char -> Int --devuelve el desplazamiento de un caracter a otro
nDesplazamiento x y = ord y - ord x  

esDescifrado :: (String,String) -> Bool
esDescifrado ((p1:pa),(p2:pb)) | cifrar (p1:pa) (nDesplazamiento p1 p2) == (p2:pb) = True 
                               | otherwise = False

hayDescifrados:: String -> [String] -> Bool
hayDescifrados s [] = False
hayDescifrados s (p:ps) | esDescifrado (s,p) = True
                        | otherwise = hayDescifrados s ps

cifradosDe:: String -> [String] -> [(String, String)] --devuelve vectores de p y sus cifrados y de p y los strings de los cuales p es cifrado 
cifradosDe p [] = []
cifradosDe p (p1:ps) | esDescifrado (p,p1) = (p, p1) : (p1,p) : cifradosDe p ps
                     | otherwise = cifradosDe p ps

-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados (p1:ps) = cifradosDe p1 ps ++ todosLosDescifrados ps 

expandirClave :: String -> Int -> String
expandirClave clave n = expandir clave n clave

-- auxiliar para manejar la recursion
-- original se mantiene constante para resetear clave 
expandir :: String -> Int -> String -> String
expandir _ 0 _ = []  -- si n es 0 devuelve una cadena vacia.
expandir [] n original = expandir original n original  -- reinicia la clave cuando se acaben los caracteres.
expandir (p:alabra) n original = p : expandir alabra (n - 1) original  -- agrega el p actual y sigue con alabra

cifrarConClaveExpandida:: String -> String -> String
cifrarConClaveExpandida [] [] = []
cifrarConClaveExpandida (s:sl) claveExp = desplazar s n : cifrarConClaveExpandida sl (tail claveExp)
            where   n = ord (head claveExp) - 97

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere palabra clave = cifrarConClaveExpandida palabra claveExp
        where claveExp =  expandirClave clave (length palabra)
    
descifrarConClaveExpandida:: String -> String -> String
descifrarConClaveExpandida [] [] = []
descifrarConClaveExpandida (s:sl) claveExp = desplazar s ((-1) * n) : descifrarConClaveExpandida sl (tail claveExp)
            where   n = ord (head claveExp) - 97

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere palabra [] = palabra --preguntar
descifrarVigenere palabra clave = descifrarConClaveExpandida palabra claveExp
        where claveExp =  expandirClave clave (length palabra)

absoluto :: Int -> Int
absoluto x
  | x < 0     = -x
  | otherwise = x

distancia :: String -> String -> Int
distancia [] [] = 0
distancia [x] [y] = absoluto (letraANatural x - letraANatural y)
distancia (x:xs) (y:ys) = absoluto (letraANatural x - letraANatural y) + distancia xs ys  --preguntar

minimo:: [Int] -> Int --borrar
minimo [x] = x
minimo [x,y] | x <= y = x
             | x > y = y
minimo (x:y:xs) | x <= y = minimo (x:xs)
                | x > y = minimo (y:xs)

-- EJ 14
peorCifrado :: String -> [String] -> String 
peorCifrado palabra [clave] = clave
peorCifrado p [c1,c2] | distancia p (cifrarVigenere p c1) <= distancia p (cifrarVigenere p c2)  = c1
                      | distancia p (cifrarVigenere p c1 ) > distancia p (cifrarVigenere p c2) = c2
peorCifrado p (c1:c2:cs) | distancia p (cifrarVigenere p c1) <= distancia p (cifrarVigenere p c2)  = peorCifrado p (c1:cs)
                         | distancia p (cifrarVigenere p c1) > distancia p (cifrarVigenere p c2) = peorCifrado p (c2:cs)

combinacionesVigenereConUnaClave::[String] -> String -> String -> [(String, String)] --devuelve la lista de vectores (msj,clave) que incluye cada msj de la lista input que al cifrar con clave devuelve cifrado
combinacionesVigenereConUnaClave [] _ _ = []
combinacionesVigenereConUnaClave (m:ms) clave cifrado | cifrarVigenere m clave == cifrado = (m, clave) : combinacionesVigenereConUnaClave ms clave cifrado
                                                      | otherwise = combinacionesVigenereConUnaClave ms clave cifrado

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)] --que onda los repetidos?
combinacionesVigenere _ [] _ = []
combinacionesVigenere [mensaje] [clave] cifrado | cifrarVigenere mensaje clave == cifrado = [(mensaje, clave)]
                                                | otherwise =  []
combinacionesVigenere mensajes (c:cs) cifrado = combinacionesVigenereConUnaClave mensajes c cifrado ++ combinacionesVigenere mensajes cs cifrado
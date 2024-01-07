module CompruebaArbol
    (esArbolDecision
    ) where

import Data.List

esArbolDecision :: [String] -> Bool
esArbolDecision cs 
    | arbolCheck cs 0 = True
    | otherwise = False

arbolCheck :: [String] -> Int -> Bool
arbolCheck [] n = if n == 2 then True else False
arbolCheck (x:xs) n = if isInfixOf "|---" x && (contarApariciones '|' x) == 1
                        then arbolCheck xs (n+1) 
                      else if isInfixOf "|---" x && (contarApariciones '|' x) /= 1 
                        then arbolCheck xs n 
                      else False

contarApariciones :: Char -> String -> Int
contarApariciones _ [] = 0  -- Caso base: la cadena está vacía, por lo que no hay apariciones.
contarApariciones c (x:xs)
    | c == x    = 1 + contarApariciones c xs  -- Si el carácter coincide, aumenta el contador y sigue buscando.
    | otherwise = contarApariciones c xs      -- Si no coincide, sigue buscando en el resto de la cadena.

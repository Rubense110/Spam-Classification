module Preprocesamiento where

import Data.Char (isAlphaNum, isSpace, isPunctuation, toLower)
import Data.List.Split (wordsBy)
import Data.List (isInfixOf)
import Data.Function ((&))

-- Función para eliminar o reemplazar caracteres especiales
removeSpecialCharacters :: String -> String
removeSpecialCharacters = filter isAlphaNum

-- Función para normalizar el texto
normalizeText :: String -> String
normalizeText = map toLower

-- Función para dividir los correos electrónicos en palabras individuales (tokenización)
tokenizeEmail :: String -> [String]
tokenizeEmail = wordsBy isSpace

-- Lista de palabras comunes (stop words) a eliminar
stopWords :: [String]
stopWords = ["the", "and", "a", "an", "in", "on"]

-- Función para eliminar palabras comunes (stop words)
removeStopWords :: [String] -> String
removeStopWords = unwords . filter (`notElem` stopWords)

-- Función para eliminar puntuación
removePunctuation :: String -> String
removePunctuation = filter (not . isPunctuation)

-- Función para eliminar números
removeNumbers :: String -> String
removeNumbers = filter (not . (`elem` "0123456789"))

-- Función para realizar el preprocesamiento completo del correo electrónico
preprocessEmail :: String -> String
preprocessEmail email =
  let cleanedEmail = removeSpecialCharacters email
      normalizedEmail = normalizeText cleanedEmail
      tokenizedEmail = tokenizeEmail normalizedEmail
      filteredEmail = removeNumbers (removePunctuation (removeStopWords tokenizedEmail))
  in filteredEmail
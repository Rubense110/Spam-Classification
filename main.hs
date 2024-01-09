import System.IO
import Data.List
import Data.Char
import System.Directory(doesFileExist)

import CompruebaArbol

main :: IO ()
main = do
    putChar '\n'
    putStrLn "######################################"
    putStrLn "        ELECCIÓN DE FICHERO"
    putStrLn "######################################"
    putChar '\n'

    putStrLn "Por favor indica el nombre del fichero de árbol"

    nombre <- getLine
    putChar '\n'
    exist <- doesFileExist nombre

    if exist then do 
        putStrLn("Cargando fichero de árbol "++nombre++"...")
        putChar '\n'

        mainMenu nombre

    else do
        putStrLn("El fichero de árbol indicado no existe, por favor, pruebe de nuevo")
        putChar '\n'
        main
     
mainMenu :: FilePath -> IO ()
mainMenu nombre = do
    contents <- readFile nombre
    if not (esArbolDecision (lines contents)) then do putStrLn "El fichero no es de árbol de decisión" 
    else do
        putChar '\n'
        putStrLn "######################################"
        putStrLn("Fichero cargado: "++nombre)
        putStrLn "######################################"
        putChar '\n'
        putStrLn("Seleccione que acción desea realizar:")
        putChar '\n'
        putStrLn("1 - Mostrar fichero")
        putStrLn("2 - Crear árbol")
        putStrLn("3 - Clasificar ejemplo")
        putChar '\n'
        putStrLn "######################################"
        putChar '\n'

        opcion <- getLine
        if opcion == "1"
            then do
                putStrLn contents
                mainMenu nombre
            else do
                if opcion == "2"
                    then do
                        creaArbol (lines contents)
                    else do
                        if opcion == "2"
                            then do
                                putStrLn contents
                                mainMenu nombre
                            else do
                                putChar '\n'
                                putStrLn "Por favor indique una opción válida"
                                putChar '\n'
                                mainMenu nombre

-------------------------------------------------------------------

type Feature = (Int, Float)

-- tipo sinónimo para la clase final al llegar a una hoja
type Class = Int

-- Deficinicion del tipo de dato arbol de decision
data DecissionTree = L Class
                   | N Feature (DecissionTree, DecissionTree)
                   deriving (Show, Eq)

creaArbol cs = creaArbolAux cs cs 1 0 []

-- crearArbolAux 
--      contents 
--      (apariciones de |) 
--      (nº de lineas encontradas)
--      (lista de lineas pilladas)

creaArbolAux lcs (c:cs) n i ns
    | i == 2 = iniciaArbol lcs cs ns
    | contarApariciones '|' c == n = creaArbolAux lcs cs n (i+1) (c:ns) 
    | otherwise = creaArbolAux lcs cs n i ns



iniciaArbol lcs cs [a,_] = putStrLn $ show (N (read feature :: Int, read valor :: Float) (izq, dcha))
    where feature = [last (contenido !! 0)]
          valor =  contenido !! 2
          contenido =  [x | x <- words a, not ('|' `elem` x || '-' `elem` x )]
          izq =  rellenaArbolIzq lcs lcs 2 
          dcha = rellenaArbolDcha cs cs 2 0

rellenaArbolIzq total lcs@(c:cs) n 
    | apariciones && noEsClase = N (read feature :: Int, read valor :: Float) (rellenaArbolIzq total lcs (n+1), rellenaArbolDcha total lcs (n+1) 0)
    | apariciones && esClase = L (read clase :: Int)
    | otherwise = rellenaArbolIzq total cs n
    where apariciones = contarApariciones '|' c == n 
          noEsClase =  not ("class" `isInfixOf` c)
          esClase = "class" `isInfixOf` c
          feature = [last (contenido !! 0)]
          valor =  contenido !! 2
          clase = last contenido
          contenido =  [x | x <- words c, not ('|' `elem` x || '-' `elem` x )]

rellenaArbolDcha total lcs@(c:cs) n i
    | apariciones && noEsClase = N (read feature :: Int, read valor :: Float) (rellenaArbolIzq total lcs (n+1), rellenaArbolDcha total lcs (n+1) 0)
    | apariciones && esClase = rellenaArbolDcha total cs n (i+1)
    | apariciones && esClase && i==1 = L (read clase :: Int)
    | otherwise = rellenaArbolDcha total lcs n 0
    where apariciones = contarApariciones '|' c == n 
          noEsClase =  not ("class" `isInfixOf` c)
          esClase = "class" `isInfixOf` c
          feature = [last (contenido !! 0)]
          valor =  contenido !! 2
          clase = last contenido
          contenido =  [x | x <- words c, not ('|' `elem` x || '-' `elem` x )]








-- N (3,0.8) ((L 2),(L 1)) 
{-
rellenaArbol ics dcs@(c:cs) n 
    | apariciones && noEsClase = N (read feature :: Int, read valor :: Float) (rellenaArbol ics ics (n+1), rellenaArbol ics dcs (n+1))
    | apariciones && esClase = L (read clase :: Int)
    | otherwise = rellenaArbol ics cs n
    where apariciones = contarApariciones '|' c == n 
          noEsClase =  not ("class" `isInfixOf` c)
          esClase = "class" `isInfixOf` c
          feature = [last (contenido !! 0)]
          valor =  contenido !! 2
          clase = last contenido
          contenido =  [x | x <- words c, not ('|' `elem` x || '-' `elem` x )] -}


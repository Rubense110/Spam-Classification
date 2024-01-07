import System.IO
import Data.List
import Data.Char
import System.Directory(doesFileExist)

import DecissionTree
import CompruebaArbol

main :: IO ()
main = do
    putChar '\n'
    putStrLn "--------------------------------------"
    putStrLn "        ELECCIÓN DE FICHERO"
    putStrLn "--------------------------------------"
    putChar '\n'

    putStrLn "Por favor indica el nombre del fichero de árbol"

    nombre <- getLine
    putChar '\n'
    exist <- doesFileExist nombre

    if exist then do 
        putStrLn("Cargando fichero de árbol "++nombre++"...")
        putChar '\n'

        leeArbol nombre

    else do
        putStrLn("El fichero de árbol indicado no existe, por favor, pruebe de nuevo")
        putChar '\n'
        main
     
leeArbol :: FilePath -> IO ()
leeArbol nombre = do
    contents <- readFile nombre
    if not (esArbolDecision (lines contents)) then putStrLn "El fichero no es de árbol de decisión" 
    else putStrLn contents -- por aqui



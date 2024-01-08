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

creaArbol cs = creaArbolAux cs 1 0 []

-- crearArbolAux 
--      contents 
--      (apariciones de |) 
--      (nº de lineas encontradas)
--      (lista de lineas pilladas)

creaArbolAux _ _ 2 ns = iniciaArbol ns
creaArbolAux (c:cs) n i ns
    | contarApariciones '|' c == n = creaArbolAux cs n (i+1) (c:ns) 
    | otherwise = creaArbolAux cs n i ns


iniciaArbol [a,_] = rellenaArbol (N (read feature :: Int, read valor :: Float) (L (-1),L (-1)))
    where feature = [last (contenido !! 0)]
          valor =  contenido !! 2
          contenido =  [x| x <- words a, not ('|' `elem` x || '-' `elem` x )]

rellenaArbol a = putStrLn (show a)

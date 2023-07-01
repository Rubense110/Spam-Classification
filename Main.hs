import Preprocesamiento
import System.Directory (listDirectory)
import System.FilePath ((</>))

-- Directorio que contiene los archivos de correo electrónico
emailDirectory :: FilePath
emailDirectory = "C:\\Users\\Usuario\\OneDrive\\Documentos\\PD\\spam"


-- Función para procesar un archivo de correo electrónico
processEmailFile :: FilePath -> IO ()
processEmailFile filePath = do
  contents <- readFile filePath
  let preprocessedEmail = Preprocesamiento.preprocessEmail contents
  putStrLn preprocessedEmail

-- Función principal
main :: IO ()
main = do
  -- Obtener la lista de archivos en el directorio de correo electrónico
  files <- listDirectory emailDirectory

  -- Procesar cada archivo de correo electrónico
  mapM_ (processEmailFile . (emailDirectory </>)) files
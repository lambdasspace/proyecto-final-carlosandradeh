import System.Environment
import System.IO
import System.Random
import Data.Char (isDigit)
import RSA

appendLinesToFile :: FilePath -> [String] -> IO ()
appendLinesToFile filePath lines = do
  mapM_ (appendFile filePath) lines

processArgs :: [String] -> IO ()
processArgs [] = putStrLn "No se proporcionaron banderas."
processArgs (flag:args)
  | flag == "-e" || flag == "--encripta" = encriptacion (args !! 0)
  | flag == "-d" || flag == "--desencripta" = desencriptacion (head args) (args !! 1) (args !! 2)
  | otherwise = putStrLn $ "Banderas desconocidas: " ++ flag

integerListToString :: [Integer] -> String
integerListToString [] = ""
integerListToString [x] = show x
integerListToString (x:xs) = show x ++ "," ++ integerListToString xs


stringToIntList :: String -> [Integer]
stringToIntList str = map read (filter (all isDigit) (wordsWhen (==',') str))
  where
    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

encriptacion :: String -> IO ()
encriptacion file = do
  putStrLn $ "Archivo proporcionado: " ++ file
  contents <- readFile file

  putStrLn "Ingresa un número para generar las llaves:"
  semilla <- getLine
  let (llavePublica, llavePrivada, n) = generaLlaves (read semilla :: Int)
  let lineas = ["\nLlave Pública: " ++ show llavePublica, "\nLlave Privada: " ++ show llavePrivada, "\nn: " ++ show n ++ "\n"]
  appendLinesToFile "llaves.txt" lineas
  putStrLn "Se ha generado el archivo con las LLAVES correctamente."

  let textoEncriptadoCadena = integerListToString $ encriptaCadena llavePublica n contents
  
  writeFile "encriptado.txt" textoEncriptadoCadena
  putStrLn "Se ha generado el archivo encriptado correctamente."

stringToList :: String -> [Integer]
stringToList str = map read (words str)

desencriptacion :: String -> String -> String -> IO ()
desencriptacion file llavePrivada n = do
  putStrLn $ "Archivo proporcionado: " ++ file
  contents <- readFile file

  let textoDesencriptadoCadena = desencriptaCadena (read llavePrivada :: Integer) (read n :: Integer) (stringToIntList contents  )
  writeFile "desencriptado.txt" textoDesencriptadoCadena
  putStrLn "Se ha generado el archivo desencriptado correctamente."



-- FUNCION PRINCIPAL
main :: IO ()
main = do
  args <- getArgs
  processArgs args

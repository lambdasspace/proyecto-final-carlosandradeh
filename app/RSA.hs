module RSA where

import System.Random

-- Funcion que da los primeros n primos 
-- con el metodo de Eratosthenes.
primos:: Integer -> [Integer]
primos n = sieve [2..n] where
    -- Agregamos todos los numeros que no sean multiplos de un primo
    sieve :: [Integer] -> [Integer]
    sieve [] = []
    sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs) 

-- Funcion para obtener dos elementos aleatorios y diferentes de una lista
elementoAleatorio :: [Integer] -> StdGen -> (Integer, [Integer], StdGen)
elementoAleatorio lista gen = (elemento, actualizada, gen1) where
    n = length lista
    (i, gen1) = randomR (0, n-1) gen         -- Indice aleaotorio
    elemento = lista !! i                    -- Elemento en el indice aleatorio
    actualizada = filter (/= elemento) lista -- Lista sin el elemento tomado

-- Funcion para generar las llaves (llavePublica, llavePrivada, n) 
generaLlaves :: Int -> (Integer, Integer, Integer)
generaLlaves semilla = (llavePublica, llavePrivada, n)
  where
    gen = mkStdGen semilla
    (primo1, prims, gen2) = elementoAleatorio (primos 50000) gen
    (primo2, _, _) = elementoAleatorio prims gen2 
    n = primo1 * primo2
    fi = (primo1 - 1) * (primo2 - 1)
    e = encuentraE 2 fi
      where
        encuentraE :: Integer -> Integer -> Integer
        encuentraE e1 fi1
          | gcd e1 fi1 == 1 = e1
          | otherwise = encuentraE (e1 + 1) fi1
    llavePublica = e
    d = encuentraD 2 e fi
      where
        encuentraD :: Integer -> Integer -> Integer -> Integer
        encuentraD d2 e2 fi2
          | (d2 * e2) `mod` fi2 == 1 = d2
          | otherwise = encuentraD (d2 + 1) e2 fi2
    llavePrivada = d
 
encripta:: Integer -> Integer -> Integer -> Integer
encripta llavePublica n mensaje = textoEncriptado where
    e = llavePublica
    textoEncriptado = encrypt mensaje e n 1
      where
        encrypt :: Integer -> Integer -> Integer -> Integer -> Integer
        encrypt _ 0 _ textoEncriptado1 = textoEncriptado1
        encrypt mensaje1 e1 n1 textoEncriptado1 = encrypt mensaje1 (e1 - 1) n1 ((textoEncriptado1 * mensaje1) `mod` n1)

desencripta:: Integer -> Integer -> Integer -> Integer
desencripta llavePrivada n textoEncriptado = decrypted where
    d = llavePrivada
    decrypted = decrypt 1 d textoEncriptado n
      where 
        decrypt :: Integer -> Integer -> Integer -> Integer -> Integer
        decrypt decrypted1 0 _ _ = decrypted1
        decrypt decrypted1 d1  textoEncriptado1 n1 = decrypt ((decrypted1 * textoEncriptado1) `mod` n1) (d1 - 1) textoEncriptado1 n1

encriptaCadena :: Integer -> Integer -> String -> [Integer]
encriptaCadena llavePublica n cadena = map (\x -> encripta llavePublica n x) codigosASCII
  where
    codigosASCII = obtenerCodigosASCII cadena
    obtenerCodigosASCII :: String -> [Integer]
    obtenerCodigosASCII = map (toInteger . fromEnum)


desencriptaCadena :: Integer -> Integer -> [Integer] -> String
desencriptaCadena llavePrivada n encriptacion = map (toEnum . fromIntegral) desencriptados
  where
    desencriptados = map (desencripta llavePrivada n) encriptacion

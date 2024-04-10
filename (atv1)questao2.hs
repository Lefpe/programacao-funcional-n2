-- Verifica se um número é primo
ehPrimo :: Int -> Bool
ehPrimo n
  | n <= 1 = False
  | otherwise = null [x | x <- [2..floor (sqrt (fromIntegral n))], n `mod` x == 0]

-- Encontrar n primo
nPrimo :: Int -> Int
nPrimo n = enesimoPrimo n 2
  where
    enesimoPrimo 1 primoAtual = primoAtual
    enesimoPrimo k primoAtual
      | ehPrimo (primoAtual + 1) = enesimoPrimo (k - 1) (primoAtual + 1)
      | otherwise = enesimoPrimo k (primoAtual + 1)

main :: IO ()
main = do
  putStrLn "Digite o valor de n:"
  nStr <- getLine
  let n = read nStr :: Int
  putStrLn $ "O " ++ show n ++ "º primo é: " ++ show (nPrimo n)

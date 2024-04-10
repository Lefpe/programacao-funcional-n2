-- Verifica se um número é primo
ehPrimo :: Int -> Bool
ehPrimo n
  | n <= 1 = False
  | otherwise = null [x | x <- [2..isqrt n], n `mod` x == 0]
  where isqrt = floor . sqrt . fromIntegral

-- Calcula o enésimo número de Fibonacci
fibonacci :: Int -> Int
fibonacci n = fibAux n 0 1
  where
    fibAux 0 a _ = a
    fibAux 1 _ b = b
    fibAux k a b = fibAux (k-1) b (a+b)

-- Encontra o enésimo número primo na sequência de Fibonacci
fibonacciPrimo :: Int -> Int
fibonacciPrimo n = nthFibonacciPrimo n 1
  where
    nthFibonacciPrimo 0 _ = error "Não existe 0º número primo na sequência de Fibonacci."
    nthFibonacciPrimo k currentFib
      | ehPrimo currentFib = if k == 1 then currentFib else nthFibonacciPrimo (k-1) (currentFib + 1)
      | otherwise = nthFibonacciPrimo k (currentFib + 1)

main :: IO ()
main = do
  putStrLn "Digite o valor de n:"
  nStr <- getLine
  let n = read nStr :: Int
  putStrLn $ "O " ++ show n ++ "º número primo na sequência de Fibonacci é: " ++ show (fibonacciPrimo n)
--refeita

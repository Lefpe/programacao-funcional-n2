--verifica se é um número perfeito--
ehPerfeito :: Int -> Bool
ehPerfeito n = n == sumDivisores n 1

sumDivisores :: Int -> Int -> Int
sumDivisores n divisor
    | divisor == n = 0
    | n `mod` divisor == 0 = divisor + sumDivisores n (divisor + 1)
    | otherwise = sumDivisores n (divisor + 1)

main :: IO ()
main = do
    putStrLn "Digite um número natural:"
    input <- getLine
    let n = read input :: Int
    if ehPerfeito n
        then putStrLn $ show n ++ " é um número perfeito."
        else putStrLn $ show n ++ " não é um número perfeito."


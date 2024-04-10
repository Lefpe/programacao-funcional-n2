-- Definição da função potencia
npotencia :: Int -> Int -> Int
npotencia _ 0 = 1
npotencia base expoente = base * npotencia base (expoente - 1)

-- Função principal para ler entrada do usuário e calcular a potência
main :: IO ()
main = do
  putStrLn "Digite a base:"
  base <- readLn
  putStrLn "Digite o expoente (positivo):"
  expoente <- readLn
  --mostrar resultado --
  let resultado = if expoente < 0 
                    then error "O expoente deve ser positivo." 
                    else npotencia base expoente
  putStrLn $ "O resultado da potência é: " ++ show resultado


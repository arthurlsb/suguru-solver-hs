import Quadrado (Quadrado, getValor, setValor, setQuadrado)
import Tabuleiro  (Tabuleiro, largura)
import Resolvedor (chamaResolvedor)

criaTabuleiro :: Tabuleiro 
criaTabuleiro = [(0,0), (0,0), (0,0), (3,1), (0,2), (0,2), (2,3), (0,3),
                 (4,0), (0,4), (0,4), (0,1), (0,2), (0,2), (0,3), (0,3),
                 (0,4), (2,4), (0,1), (0,1), (0,2), (0,7), (0,7), (0,3),
                 (0,4), (1,5), (5,5), (0,1), (0,6), (1,6), (5,7), (0,7),
                 (0,8), (2,8), (0,5), (0,5), (0,6), (0,11), (0,11), (0,7),
                 (0,9), (0,8), (0,8), (0,5), (4,6), (0,12), (0,11), (4,11), 
                 (0,9), (0,9), (0,8), (0,10), (0,6), (3,12), (0,12), (0,11),
                 (0,9), (5,9), (0,10), (0,10), (0,10), (5,10), (0,12), (0,12)]

-- Transforma um quadrado numa String para printar
squareParaString :: Quadrado -> String
squareParaString (v, _)  = " " ++ show v ++ " "

-- Printa o Tabuleiro no terminal
exibirTabuleiro :: Tabuleiro -> IO ()
exibirTabuleiro b = putStrLn ("\n" ++ exibirTabuleiro' b ((largura b)-1) ((largura b)-1))
        where
            exibirTabuleiro' [] _ _    = ""
            exibirTabuleiro' (a:b) n 0 = squareParaString a ++ "\n" ++ exibirTabuleiro' b n n
            exibirTabuleiro' (a:b) n m = squareParaString a ++ exibirTabuleiro' b n (m-1)

main = do
    putStrLn "\nTabuleiro Inicial:"
    exibirTabuleiro criaTabuleiro
    putStrLn "Tabuleiro Resolvido:"
    mapM_ exibirTabuleiro (chamaResolvedor criaTabuleiro)
    putStrLn "Fim!"


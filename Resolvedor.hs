module Resolvedor (chamaResolvedor) where
import Quadrado ( Quadrado, setValor, ehVazio, getValor, getNumArea)
import Tabuleiro ( insereValor, pegaQuadrado, largura, coordVazia, Tabuleiro, numElementosArea) 
import Data.List ( (\\) )

-- Função chamada externamente que invoca o resolvedor
chamaResolvedor :: Tabuleiro -> [Tabuleiro]
chamaResolvedor b = resolvedor b (0,0) []

-- Tenta resolver o tabuleiro, caso seja possível retorna as soluções válidas numa lista, caso
-- contrário retorna uma lista vazia
resolvedor :: Tabuleiro -> (Int, Int) -> [Tabuleiro] -> [Tabuleiro]
resolvedor b (y,x) l | x == (largura b)   = resolvedor b (y+1, 0) l
                     | y == (largura b)   = b:l
                     | coordVazia b (y,x) = resolvedor' b (y,x) l (getPossibilidades b (y,x))
                     | otherwise       = resolvedor b (y,x+1) l 
           
-- Chamado dentro do método resolvedor (backtracking)
resolvedor' :: Tabuleiro -> (Int, Int) -> [Tabuleiro] -> [Int] -> [Tabuleiro]
resolvedor' b (y,x) l []    = l
resolvedor' b (y,x) l (h:t) = do
                             l1 <- [resolvedor (insereValor b (y,x) h) (y,x+1) l]
                             resolvedor' b (y,x) l1 t

-- Retorna as possibilidades de valor para uma determinada coordenada
getPossibilidades :: Tabuleiro -> (Int, Int) -> [Int]
getPossibilidades b (y,x) = [1..numElementosArea b (getNumArea(pegaQuadrado b (y,x)))] \\  (getNumerosArea b b (y,x) ++ getNumerosAdj b (y,x))

-- Retorna os valores que estão na mesma área de uma determinada coordenada
getNumerosArea :: Tabuleiro -> Tabuleiro -> (Int, Int) -> [Int]
getNumerosArea tabuleiro [] (_, _) = []
getNumerosArea tabuleiro (a:b) (y,x) | getNumArea a == getNumArea(pegaQuadrado tabuleiro (y,x)) = getValor a : getNumerosArea tabuleiro b (y, x) 
                                     | otherwise = getNumerosArea tabuleiro b (y,x)

-- Retorna os valores em campos adjacentes, ortogonalmente ou diagonalmente, de uma determinada coordenada
getNumerosAdj :: Tabuleiro -> (Int, Int) -> [Int]
getNumerosAdj b (y,x) = (left b (y,x)) : (topLeft b (y,x)) : (top b (y,x)) : (topRight b (y,x)) : (right b (y,x)) : (bottomRight b (y,x)) : (bottom b (y,x)) : (bottomLeft b (y,x)) : []

-- Retorna o número à esquerda de uma determinada coordenada
left :: Tabuleiro -> (Int, Int) -> Int
left b (y,x) | x == 0 = 0
             | otherwise = getValor (pegaQuadrado b (y,x-1))  

-- Retorna o número acima de uma determinada coordenada
top :: Tabuleiro -> (Int, Int) -> Int 
top b (y,x) | y == 0 = 0
            | otherwise = getValor (pegaQuadrado b (y-1,x))

-- Retorna o número abaixo de uma determinada coordenada
bottom :: Tabuleiro -> (Int, Int) -> Int
bottom b (y,x) | y == ((largura b) - 1) = 0
               | otherwise = getValor (pegaQuadrado b (y+1,x))

-- Retorna o número à direita de uma determinada coordenada
right :: Tabuleiro -> (Int, Int) -> Int
right b (y,x) | x == ((largura b) - 1) = 0
              | otherwise = getValor (pegaQuadrado b (y,x+1))

-- Retorna o número na diagonal esquerda superior de uma determinada coordenada
topLeft :: Tabuleiro -> (Int, Int) -> Int
topLeft b (y, x) | x == 0 || y == 0 = 0
                 | otherwise = getValor (pegaQuadrado b (y-1, x-1))

-- Retorna o número na diagonal direita superior de uma determinada coordenada
topRight :: Tabuleiro -> (Int, Int) -> Int
topRight b (y, x) | x == ((largura b) - 1) || y == 0 = 0
                  | otherwise = getValor (pegaQuadrado b (y-1, x+1))

-- Retorna o número na diagonal esquerda inferior de uma determinada coordenada
bottomLeft :: Tabuleiro -> (Int, Int) -> Int
bottomLeft b (y, x) | x == 0 || y == ((largura b) - 1) = 0
                    | otherwise = getValor (pegaQuadrado b (y+1, x-1))

-- Retorna o número na diagonal direita inferior de uma determinada coordenada
bottomRight :: Tabuleiro -> (Int, Int) -> Int
bottomRight b (y, x) | x == ((largura b) - 1) || y == ((largura b) - 1) = 0
                     | otherwise = getValor (pegaQuadrado b (y+1, x+1))


                    

module Tabuleiro (Tabuleiro, largura, indice, pegaQuadrado, insereValor, coordVazia, numElementosArea) where

import Quadrado ( Quadrado, setValor, ehVazio, getNumArea)

-- Tabuleiro é uma lista de quadrados
type Tabuleiro = [Quadrado]

-- Conta e retorna a quantidade de quadrados em uma determinada área
numElementosArea :: Tabuleiro -> Int -> Int 
numElementosArea [] x = 0
numElementosArea (a:b) x | getNumArea a == x = numElementosArea b x + 1
                                 | otherwise = numElementosArea b x
-- Retorna a largura do tabuleiro 
largura :: Tabuleiro -> Int
largura b = round (sqrt (fromIntegral(length b)))

-- Pega a coordenada e retorna o índice equivalente da lista
indice :: Tabuleiro -> (Int, Int) -> Int
indice b (y,x) = y * largura b + x

-- Retorna o quadrado presente em uma determinada coordenada
pegaQuadrado :: Tabuleiro -> (Int, Int) -> Quadrado
pegaQuadrado b (y,x) = b !! indice b (y,x)

-- Insere o valor em uma determinada coordenada e retorna um tabuleiro novo
insereValor :: Tabuleiro -> (Int, Int) -> Int -> Tabuleiro
insereValor b (y,x) v = insereValor' b (indice b (y,x)) v
    where
        insereValor' (a:b) i v | i == 0 = setValor a v:b
                         | otherwise = a:insereValor' b (i-1) v

-- Verifica se uma determinada coordenada é vazia
coordVazia :: Tabuleiro -> (Int, Int) -> Bool
coordVazia b (y,x) = ehVazio (pegaQuadrado b (y,x))
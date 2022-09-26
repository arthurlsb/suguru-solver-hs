module Quadrado (Quadrado, getValor, setValor, setQuadrado, ehVazio, getNumArea) where

-- Quadrado é uma tupla na qual um "Int" representa o valor presente no quadrado e o outro "Int" a área na qual o quadrado pertence
type Quadrado = (Int, Int)

-- Retorna o valor de um determinado quadrado
getValor :: Quadrado -> Int
getValor (v, _) = v

-- Recebe um valor e coloca em um quadrado
setValor :: Quadrado -> Int -> Quadrado
setValor (_, b) v = (v, b)

-- Cria um quadrado com um valor e o número de sua área
setQuadrado :: Int -> Int -> Quadrado
setQuadrado v a  = (v, a)

-- Retorna o número da área de um determinado quadrado
getNumArea :: Quadrado -> Int
getNumArea (_, a) = a

-- Retorna true se o valor de um determinado quadrao for 0 (quadrado vazio)
ehVazio :: Quadrado -> Bool
ehVazio (v, _) = (v == 0) 
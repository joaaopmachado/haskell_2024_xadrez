module Tabuleiro
( Tabuleiro
, tabuleiroInicial
, mostrarTabuleiro
, legendas
,Peca(..)
,Cor(..)
,Posicao
) where

import Control.Monad
type Tabuleiro = [[Char]]
type Posicao = (Int, Int)
data Cor = Branca | Preta deriving (Show, Eq)

data Peca = Rei Cor
          | Rainha Cor
          | Bispo Cor
          | Cavalo Cor
          | Torre Cor
          | Peao Cor
          deriving (Eq, Show)


tabuleiroInicial :: Tabuleiro
tabuleiroInicial = 
    [ "rnbqkbnr"
    , "pppppppp"
    , "        "
    , "        "
    , "        "
    , "        "
    , "PPPPPPPP"
    , "RNBQKBNR"
    ]

-- Função que será utilizada para mostrar o tabuleiro na tela
mostrarTabuleiro :: Tabuleiro -> IO ()
mostrarTabuleiro tab = do
    putStrLn "   a b c d e f g h"
    putStrLn "  -----------------"
    forM_ (zip ([8,7..1] :: [Int]) tab) $ \(num, linha) -> do
        putStr $ show num ++ " |"
        mapM_ (\c -> putStr $ ' ' : c : []) linha
        putStrLn (" | " ++ show num)
    putStrLn "  -----------------"
    putStrLn "   a b c d e f g h"

-- Legenda com as peças do jogo
legendas :: IO ()
legendas = do
    putStrLn "\nLegendas:"
    putStrLn "R/r - Torre (Rook)"
    putStrLn "N/n - Cavalo (Knight)"
    putStrLn "B/b - Bispo (Bishop)"
    putStrLn "Q/q - Rainha (Queen)"
    putStrLn "K/k - Rei (King)"
    putStrLn "P/p - Peão (Pawn)"
    putStrLn "(Letras maiúsculas: brancas, letras minúsculas: pretas)"  

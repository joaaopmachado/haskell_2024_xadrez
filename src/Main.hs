import Tabuleiro
import ProcessarMovimento


estadoInicial :: (Tabuleiro, Cor)
estadoInicial = (tabuleiroInicial, Branca)

alternaJogador :: Cor -> Cor
alternaJogador Branca = Preta
alternaJogador Preta = Branca

loopJogo :: (Tabuleiro, Cor) -> IO ()
loopJogo estado@(tab, cor) = do
    mostrarTabuleiro tab
    putStrLn $ "Vez das " ++ show cor ++ ". Digite seu movimento ou 'sair' para encerrar:"
    entrada <- getLine
    if entrada == "sair"
        then putStrLn "Fim do jogo!"
        else case validaEntrada entrada of
            Left errMsg -> do
                putStrLn errMsg
                loopJogo estado
            Right _ -> 
                case processarMovimento entrada tab cor of
                    Just novoTabuleiro -> do
                        loopJogo (novoTabuleiro, alternaJogador cor)
                    Nothing -> do
                        putStrLn "Movimento inválido!"
                        loopJogo estado




validaEntrada :: String -> Either String Bool
validaEntrada [c1,r1,c2,r2] 
    | c1 `notElem` colunasValidas = Left "Coluna inicial inválida."
    | c2 `notElem` colunasValidas = Left "Coluna de destino inválida."
    | r1 `notElem` linhasValidas  = Left "Linha inicial inválida."
    | r2 `notElem` linhasValidas  = Left "Linha de destino inválida."
    | otherwise                   = Right True
validaEntrada _ = Left "Formato de entrada inválido. Use o formato 'e2e4'."

colunasValidas :: String
colunasValidas = "abcdefgh"

linhasValidas :: String
linhasValidas = "12345678"

main :: IO ()
main = do
    putStrLn "Vamos iniciar o jogo de Xadrez!"
    loopJogo estadoInicial

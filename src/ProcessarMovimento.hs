    module ProcessarMovimento
    ( 
        processarMovimento
    ) where

    import Data.Char (isLower, isUpper)
    import Debug.Trace (trace)
    import Tabuleiro (Tabuleiro, Cor(..))


    -- Converta a letra da coluna para um índice
    colunaParaIndice :: Char -> Int
    colunaParaIndice col = fromEnum col - fromEnum 'a'

    -- Converta o número da linha para um índice
    linhaParaIndice :: Char -> Int
    linhaParaIndice linha = 8 - (read [linha] :: Int)


    pecaNaPosicao :: (Int, Int) -> Tabuleiro -> (Char, Maybe Cor)
    pecaNaPosicao (col, linha) tab = case drop linha tab of
        (linhaSelecionada : _) -> case drop col linhaSelecionada of
            (peca : _) -> (peca, if isUpper peca then Just Branca else if isLower peca then Just Preta else Nothing)
            _ -> error "Coluna inválida"
        _ -> error "Linha inválida"

    movimentoValido :: Char -> (Int, Int) -> (Int, Int) -> Tabuleiro -> Cor -> Bool
    movimentoValido peca inicio fim tab corAtual = 
        let (pecaDestino, corDestino) = pecaNaPosicao fim tab
        in case peca of
            'K' -> abs (fst inicio - fst fim) <= 1 && abs (snd inicio - snd fim) <= 1 && corDestino /= Just corAtual
            'k' -> abs (fst inicio - fst fim) <= 1 && abs (snd inicio - snd fim) <= 1 && corDestino /= Just corAtual

            'P' -> fst inicio == fst fim && ((snd fim == snd inicio - 1 && fst (pecaNaPosicao fim tab) == ' ')
                        || (snd inicio == 6 && snd fim == snd inicio - 2 && all (\y -> fst (pecaNaPosicao (fst inicio, y) tab) == ' ') [snd inicio - 1, snd fim]))
                        || abs (fst inicio - fst fim) == 1 && snd fim == snd inicio - 1 && corDestino == Just Preta
            'p' -> fst inicio == fst fim && ((snd fim == snd inicio + 1 && fst (pecaNaPosicao fim tab) == ' ')
                        || (snd inicio == 1 && snd fim == snd inicio + 2 && all (\y -> fst (pecaNaPosicao (fst inicio, y) tab) == ' ') [snd inicio + 1, snd fim]))
                        || abs (fst inicio - fst fim) == 1 && snd fim == snd inicio + 1 && corDestino == Just Branca

            'N' -> ((abs (fst inicio - fst fim) == 1 && abs (snd inicio - snd fim) == 2) || 
                    (abs (fst inicio - fst fim) == 2 && abs (snd inicio - snd fim) == 1)) && corDestino /= Just corAtual
            'n' -> ((abs (fst inicio - fst fim) == 1 && abs (snd inicio - snd fim) == 2) || 
                    (abs (fst inicio - fst fim) == 2 && abs (snd inicio - snd fim) == 1)) && corDestino /= Just corAtual

            'R' -> (fst inicio == fst fim || snd inicio == snd fim) && caminhoLivre inicio fim tab && corDestino /= Just corAtual
            'r' -> (fst inicio == fst fim || snd inicio == snd fim) && caminhoLivre inicio fim tab && corDestino /= Just corAtual

            'B' -> abs (fst inicio - fst fim) == abs (snd inicio - snd fim) && caminhoLivre inicio fim tab && corDestino /= Just corAtual
            'b' -> abs (fst inicio - fst fim) == abs (snd inicio - snd fim) && caminhoLivre inicio fim tab && corDestino /= Just corAtual

            'Q' -> (fst inicio == fst fim || snd inicio == snd fim || abs (fst inicio - fst fim) == abs (snd inicio - snd fim)) && caminhoLivre inicio fim tab && corDestino /= Just corAtual
            'q' -> (fst inicio == fst fim || snd inicio == snd fim || abs (fst inicio - fst fim) == abs (snd inicio - snd fim)) && caminhoLivre inicio fim tab && corDestino /= Just corAtual

            _   -> False

    processarMovimento :: String -> Tabuleiro -> Cor -> Maybe Tabuleiro
    processarMovimento [c1, r1, c2, r2] tab corAtual =
        let inicio = (colunaParaIndice c1, linhaParaIndice r1)
            fim    = (colunaParaIndice c2, linhaParaIndice r2)
            peca   = fst (pecaNaPosicao inicio tab)
            inicioStr = trace ("Coordenada de início: " ++ show inicio) inicio
            fimStr = trace ("Coordenada de fim: " ++ show fim) fim
            pecaStr = trace ("Peça na posição de início: " ++ [peca]) peca
        in 
            if movimentoValido pecaStr inicioStr fimStr tab corAtual
            then trace "Movimento é válido" (Just (moverPeca inicioStr fimStr tab))
            else trace "Movimento é inválido" Nothing
    processarMovimento _ _ _ = trace "Formato de entrada inválido" Nothing


    moverPeca :: (Int, Int) -> (Int, Int) -> Tabuleiro -> Tabuleiro
    moverPeca (x1,y1) (x2,y2) tab = 
        let linhaIni = (tab !! y1)
            linhaFim = (tab !! y2)
            novaLinhaIni = take x1 linhaIni ++ ['.'] ++ drop (x1 + 1) linhaIni
            novaLinhaFim = take x2 linhaFim ++ [(linhaIni !! x1)] ++ drop (x2 + 1) linhaFim
            tabuleiroParcial = take y1 tab ++ [novaLinhaIni] ++ drop (y1 + 1) tab
        in take y2 tabuleiroParcial ++ [novaLinhaFim] ++ drop (y2 + 1) tabuleiroParcial

    caminhoLivre :: (Int, Int) -> (Int, Int) -> Tabuleiro -> Bool
    caminhoLivre (x1, y1) (x2, y2) tab
        | x1 == x2 = all (\y -> fst (pecaNaPosicao (x1, y) tab) == ' ') [min y1 y2 + 1 .. max y1 y2 - 1]
        | y1 == y2 = all (\x -> fst (pecaNaPosicao (x, y1) tab) == ' ') [min x1 x2 + 1 .. max x1 x2 - 1]
        | otherwise = all (\(x, y) -> fst (pecaNaPosicao (x, y) tab) == ' ') $ caminhoDiagonal (x1, y1) (x2, y2)


    caminhoDiagonal :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
    caminhoDiagonal (x1, y1) (x2, y2) = 
        tail $ takeWhile (/= (x2, y2)) $ iterate next (x1, y1)
        where
            next (x, y) = (if x2 > x1 then succ x else pred x, if y2 > y1 then succ y else pred y)
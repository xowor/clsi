import CLSI

test :: IO ()
test =
  do
    putStrLn "#######################################################"
    putStrLn "q :  Quit"
    putStrLn "0 :  Esempio slides"
    putStrLn "1 :  Esempio slides ricorsivo"
    putStrLn "2 :  Esercizio 1: applicazione regola a) ad input 1)"
    putStrLn "3 :  Esercizio 1: applicazione regola a) ad input 2)"
    putStrLn "4 :  Esercizio 1: applicazione regola b) ad input 1)"
    putStrLn "5 :  Esercizio 1: applicazione regola b) ad input 2)"
    putStrLn "#######################################################"
    x <- getLine
    if (x == "q")
      then
        putStrLn ""
      else
        do
          testH x
          putStrLn "\n"
          test

testH :: String -> IO ()
testH tn | tn == "0"  =
          do
            putStrLn (">>> Esempio slides")
            putStrLn ("    input         :: " ++ (showTerm (exampleInput)))
            putStrLn ("    sx rule       :: " ++ (showPattern (exampleRewriteStart)))
            putStrLn ("    dx rule       :: " ++ (showPattern (exampleRewriteEnd)))
            putStrLn ("    evaluation    :: " ++ (showVars (evalC exampleInput exampleRewriteStart)))
            putStrLn ("    application   :: " ++ (showMaybeTerm (apply exampleInput exampleRewrite)))
        | tn == "1"  =
          do
            putStrLn (">>> Esempio slides ricorsivo")
            putStrLn ("    input 1       :: " ++ (showTerm (exampleInput)))
            putStrLn ("    sx rule       :: " ++ (showPattern (exampleRewriteStart)))
            putStrLn ("    dx rule       :: " ++ (showPattern (exampleRewriteEnd)))
            putStrLn ("    evaluation 1  :: " ++ (showVars (evalC exampleInput exampleRewriteStart)))
            putStrLn ("    application 1 :: " ++ (showMaybeTerm (apply exampleInput exampleRewrite)))
            case (apply exampleInput exampleRewrite) of
              Just input2 ->
                do
                  putStrLn ("    input 2       :: " ++ (showTerm input2))
                  putStrLn ("    evaluation 2  :: " ++ (showVars (evalC input2 exampleRewriteStart)))
                  putStrLn ("    application 2 :: " ++ (showMaybeTerm (apply input2 exampleRewrite)))
        | tn == "2"  =
          do
            putStrLn (">>> Esercizio 1: applicazione regola a) ad input 1)")
            putStrLn ("    input         :: " ++ (showTerm (es1Input)))
            putStrLn ("    sx rule       :: " ++ (showPattern (esARewriteStart)))
            putStrLn ("    dx rule       :: " ++ (showPattern (esARewriteEnd)))
            putStrLn ("    evaluation    :: " ++ (showVars (evalC es1Input esARewriteStart)))
            putStrLn ("    application   :: " ++ (showMaybeTerm (apply es1Input esARewrite)))
        | tn == "3"  =
          do
            putStrLn (">>> Esercizio 1: applicazione regola a) ad input 2)")
            putStrLn ("    input         :: " ++ (showTerm (es2Input)))
            putStrLn ("    sx rule       :: " ++ (showPattern (esARewriteStart)))
            putStrLn ("    dx rule       :: " ++ (showPattern (esARewriteEnd)))
            putStrLn ("    evaluation    :: " ++ (showVars (evalC es2Input esARewriteStart)))
            putStrLn ("    application   :: " ++ (showMaybeTerm (apply es2Input esARewrite)))
        | tn == "4"  =
          do
            putStrLn (">>> Esercizio 1: applicazione regola b) ad input 1)")
            putStrLn ("    input         :: " ++ (showTerm (es1Input)))
            putStrLn ("    sx rule       :: " ++ (showPattern (esBRewriteStart)))
            putStrLn ("    dx rule       :: " ++ (showPattern (esBRewriteEnd)))
            putStrLn ("    evaluation    :: " ++ (showVars (evalC es1Input esBRewriteStart)))
            putStrLn ("    application   :: " ++ (showMaybeTerm (apply es1Input esBRewrite)))
        | tn == "5"  =
          do
            putStrLn (">>> Esercizio 1: applicazione regola b) ad input 2)")
            putStrLn ("    input         :: " ++ (showTerm (es2Input)))
            putStrLn ("    sx rule       :: " ++ (showPattern (esBRewriteStart)))
            putStrLn ("    dx rule       :: " ++ (showPattern (esBRewriteEnd)))
            putStrLn ("    evaluation    :: " ++ (showVars (evalC es2Input esBRewriteStart)))
            putStrLn ("    application   :: " ++ (showMaybeTerm (apply es2Input esBRewrite)))
        | otherwise = putStrLn (show ("Option not valid"))

          where
            -- Esempio slides
            exampleInput = PC (
                              LC (Seq (Alpha 'a') (Alpha 'c')) (LC (Seq (Alpha 'a') (Alpha 'b')) (TSequence (Alpha 'd')))
                            ) (
                              LC (Seq (Alpha 'a') (Alpha 'b')) (LC (Seq (Alpha 'a') (Alpha 'c')) (TSequence Nil))
                            )
            exampleRewriteStart = (PPC (
                                  PLC (SPSPSeq (SPAlpha 'a') (SPVar 'x')) (PVar 'X')
                                ) (
                                  PLC (SPSPSeq (SPAlpha 'a') (SPVar 'y')) (PVar 'Y')
                                ))
            exampleRewriteEnd = (
                                  PLC (
                                    SPSPSeq (SPSPSeq (SPSPSeq (SPAlpha 'a') (SPVar 'x')) (SPAlpha 'a')) (SPVar 'y')
                                  ) (
                                    PPC (PVar 'X') (PVar 'Y')
                                  )
                                )
            exampleRewrite = (Rewrite exampleRewriteStart exampleRewriteEnd)

            testInput1 = (PC (TSequence (Seq (Alpha 'a') (Alpha 'c'))) (TSequence (Alpha 'd')))
            rewriteStart1 = (
                              PPC (PVar 'X') (PVar 'Y')
                            )
            rewriteEnd1 = (
                              PPC (PVar 'Y') (PVar 'X')
                            )
            rewrite1 = Rewrite rewriteStart1 rewriteEnd1
            testInput2 = PC (
                              LC (Seq (Alpha 'a') (Alpha 'c')) (LC (Seq (Alpha 'a') (Alpha 'b')) (TSequence (Alpha 'd')))
                            ) (
                              LC (Seq (Alpha 'a') (Alpha 'b')) (TSequence (Seq (Alpha 'a') (Alpha 'c')))
                            )
            rewriteStart2 = (PPC (
                              PLC (SPSPSeq (SPAlpha 'a') (SPVar 'x')) (PVar 'X')
                            ) (
                              PLC (SPSPSeq (SPAlpha 'a') (SPVar 'y')) (PVar 'Y')
                            ))
            rewriteEnd2 = (
                            PSequencePattern (
                              SPSPSeq (SPSPSeq (SPSPSeq (SPAlpha 'a') (SPAlpha 'x')) (SPAlpha 'a')) (SPAlpha 'y')
                            )
                          )
            rewrite2 = Rewrite rewriteStart1 rewriteEnd1
            rewriteExerciseStartA = PSequencePattern (SPSPSeq (SPSPSeq (SPSPSeq (SPAlpha 'a') (SPVar 'x')) (SPAlpha 'a')) (SPVar 'z'))
            rewriteExerciseEndA = PSequencePattern (SPSPSeq (SPSPSeq (SPSPSeq (SPAlpha 'c') (SPVar 'x')) (SPAlpha 'd')) (SPVar 'z'))
            rewriteExerciseA = Rewrite rewriteExerciseStartA rewriteExerciseEndA

            -- Esercizio 1
            es1Input =  LC (
                          Seq (Alpha 'a') (Seq (Alpha 'c') (Alpha 'b'))
                        ) (
                          PC (TSequence (Alpha 'd')) (PC (TSequence (Seq (Alpha 'a') (Seq (Alpha 'd') (Alpha 'b')))) (
                            TSequence (Seq (Alpha 'a') (Seq (Alpha 'd') (Seq (Alpha 'b') (Alpha 'b'))))
                          ))
                        )
            es2Input =  LC (
                          Seq (Alpha 'e') (Seq (Alpha 'a') (Seq (Alpha 'c') (Seq (Alpha 'b') (Alpha 'd'))))
                        ) (
                          PC (
                            TSequence (Seq (Alpha 'e') (Seq (Alpha 'a') (Seq (Alpha 'd') (Alpha 'b'))))
                          ) (
                            TSequence (Seq (Alpha 'a') (Seq (Alpha 'd') (Seq (Alpha 'a') (Alpha 'b'))))
                          )
                        )
            esARewriteStart = PSequencePattern (
                                SPSPSeq (SPAlpha 'a') (SPSPSeq (SPVar 'x') (SPSPSeq (SPAlpha 'b') (SPSVar 's')))
                              )
            esARewriteEnd = PSequencePattern (
                                SPSPSeq (SPAlpha 'c') (SPSPSeq (SPVar 'x') (SPSPSeq (SPAlpha 'd') (SPSVar 's')))
                              )
            esARewrite = Rewrite esARewriteStart esARewriteEnd
            esBRewriteStart = PLC (
                                SPSPSeq (SPAlpha 'a') (SPSPSeq (SPVar 'x') (SPSPSeq (SPAlpha 'b') (SPSVar 's')))
                              ) (
                                PPC (PVar 'X') (PVar 'Y')
                              )
            esBRewriteEnd = PPC (
                                PLC (
                                  SPSPSeq (SPAlpha 'a') (SPVar 'x')
                                ) (
                                  PVar 'X'
                                )
                              ) (
                                PLC (
                                  SPSPSeq (SPSVar 's') (SPAlpha 'b')
                                ) (
                                  PVar 'Y'
                                )
                              )
            esBRewrite = Rewrite esBRewriteStart esBRewriteEnd

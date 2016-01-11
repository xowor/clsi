module CLSI (
  Expr (ETerm, ESequence),
  Term (TSequence, LC, PC),
  Sequence (Nil, Alpha, Seq),
  Pattern (PSequencePattern, PLC, PPC, PTerm, PVar),
  SequencePattern (SPNil, SPAlpha, SPSPSeq, SPSVar, SPVar),
  Rewrite (Rewrite ),
  apply,
  eval,
  evalC,
  showMaybeTerm,
  showTerm,
  showSequence,
  showPattern,
  showSequencePattern,
  showExpr,
  showVars,
  showRewrite
) where


  import Data.Typeable
  import Data.Char
  import Data.ByteString.Char8 (putStr)

  data Expr
    = ETerm Term
    | ESequence Sequence
      deriving (Show)

  data Term                                       -- T
    = TSequence Sequence                          -- S
    | LC Sequence Term                            -- (S)└ _| T
    | PC Term Term                                -- T | T
      deriving (Show)

  data Sequence                                   -- S
    = Nil                                         -- ε
    | Alpha Char                                  -- [a..z]
    | Seq Sequence Sequence                       -- S • S
      deriving (Show)

  data Pattern                                    -- P
    = PSequencePattern SequencePattern            -- SP
    | PLC SequencePattern Pattern                 -- (SP)└ _| P
    | PPC Pattern Pattern                         -- P | P
    | PTerm Term                                  -- X
    | PVar Char                                   -- X
      deriving (Show)

  data SequencePattern                            -- SP
    = SPNil                                       -- ε
    | SPAlpha Char                                -- [a..z]
    | SPSPSeq SequencePattern SequencePattern     -- SP • SP
    | SPSVar Char                                 -- x~
    | SPVar Char                                  -- x
      deriving (Show)


  data Rewrite
    = Rewrite Pattern Pattern                     -- P -> P'
      deriving (Show)



  -- Applies a rewrite to a term. Context is detected automatically.
  apply :: Term -> Rewrite -> Maybe Term
  apply t (Rewrite p1 p2) = applyContext t (Rewrite p1 p2)


  -- Applies a rewrite to a term. First of all, the application is tried to the
  -- whole context; if fails, the application is tried recursively.
  -- Variables evaluation is done on empty context (and then on each recursion).
  -- If no application is successful, Nothing is returned. Othrewise, the
  -- output of the application is returned.
  applyContext :: Term -> Rewrite -> Maybe Term
  applyContext t (Rewrite p1 p2) =
    -- Application on empty context
    case (checkAndApplyPattern p2 (eval t p1)) of
      Just t -> Just t
      Nothing ->
        case t of
          -- Application on loop&containment
          LC s ta ->
            case (applyContext ta (Rewrite p1 p2)) of
              Just t1 -> Just (LC s t1)
              Nothing -> Nothing
          -- Application on parallel composition (both left and right)
          PC ta tb ->
            case (applyContext ta (Rewrite p1 p2)) of
              Just t1 ->
                case (applyContext tb (Rewrite p1 p2)) of
                  Just t2 -> Just (PC t1 t2)
                  Nothing -> Just (PC t1 tb)
              Nothing ->
                case (applyContext tb (Rewrite p1 p2)) of
                  Just t2 -> Just (PC ta t2)
                  Nothing -> Nothing
          otherwise -> Nothing


  -- Checks the variables list and applies them to the given pattern, returning
  -- a term if the application is successful, Nothing otherwise.
  -- If the variables list is nothing, an Nothing is returned.
  checkAndApplyPattern :: Pattern -> Maybe [(Char, Expr)] -> Maybe Term
  checkAndApplyPattern pattern variables =
    case variables of
      Just vs ->
        case (applyPattern pattern vs) of
          Just term -> Just term
          Nothing -> Nothing
      Nothing -> Nothing


  -- Applies the given variables list to the given pattern. If the application is
  -- succesful, returns the term. Otherwise, Nothing is returned.
  applyPattern :: Pattern -> [(Char, Expr)] -> Maybe Term
  applyPattern p vs =
    case p of
      PSequencePattern sequencePattern ->
        case (applySequencePattern sequencePattern vs) of
          Just sequence -> Just (TSequence sequence)

      PLC sequencePattern pattern ->
        case (applySequencePattern sequencePattern vs) of
          Just sequence ->
            case (applyPattern pattern vs) of
              Just term -> Just (LC sequence term)

      PPC pattern1 pattern2 ->
        case (applyPattern pattern1 vs) of
          Just term1 ->
            case (applyPattern pattern2 vs) of
              Just term2 -> Just (PC term1 term2)

      PTerm term -> Just (term)
      PVar c ->
        case (value vs c) of
          ETerm term -> Just term
          ESequence seq -> Just (TSequence seq)


  applySequencePattern :: SequencePattern -> [(Char, Expr)] -> Maybe Sequence
  applySequencePattern se vs =
    case se of
      SPNil -> Just (Nil)
      SPAlpha c -> Just (Alpha c)
      SPSPSeq sp1 sp2 ->
        case (applySequencePattern sp1 vs) of
          Just sequence1 ->
            case (applySequencePattern sp2 vs) of
              Just sequence2 -> Just (Seq sequence1 sequence2)

      SPSVar c -> -- Just (sequence)
        case (value vs c) of
          ETerm term -> Nothing
          ESequence seq -> Just seq
      SPVar c ->
        case (value vs c) of
          ETerm term -> Nothing
          ESequence seq -> Just seq


  -- Extracts a variable from a list of evaluated variables, given the variable
  -- label.
  value :: [(Char, Expr)] -> Char -> Expr
  value ls c = valueAux c [y | (x, y) <- ls, x == c]

  valueAux :: Char -> [Expr] -> Expr
  valueAux c ss | (length ss) > 0  =  ss !! 0
                | otherwise  =  ESequence (Alpha c)

  -- ########################################################


  -- #### EVALUATION ########################################

  -- Evaluates a term with a pattern, returning the variables value list if the
  -- pattern mathces the term. Otherwise, Nothing is returned.
  eval :: Term -> Pattern -> Maybe [(Char, Expr)]
  eval t p = evalPattern t p


  -- Performs evaluation recursively on each context. For debug only.
  evalC :: Term -> Pattern -> Maybe [(Char, Expr)]
  evalC t p =
    -- Application on entire context
    case (eval t p) of
      Just vars -> Just vars
      Nothing ->
        case t of
          LC s ta -> evalC ta p
          PC ta tb -> (evalC ta p) ?++ (evalC tb p)
          otherwise -> Nothing


  evalPattern :: Term -> Pattern -> Maybe [(Char, Expr)]
  evalPattern te pe =
    case pe of
      PSequencePattern sequencePattern ->
        case te of
          TSequence sequence -> (evalSequencePattern sequence sequencePattern)
          otherwise -> Nothing
      PLC sequencePattern pattern ->
        case te of
          LC sequence term -> ((evalSequencePattern sequence sequencePattern) ?++! (evalPattern term pattern))
          otherwise -> Nothing
      PPC pattern1 pattern2 ->
        case te of
          PC term1 term2 -> ((evalPattern term1 pattern1) ?++! (evalPattern term2 pattern2))
          otherwise -> Nothing
      PTerm term -> Just []
      PVar c -> Just [(c, (ETerm te))]


  evalSequencePattern :: Sequence -> SequencePattern -> Maybe [(Char, Expr)]
  evalSequencePattern se sp =
    case sp of
      SPNil ->
        case se of
          Nil -> Just []
          otherwise -> Nothing
      SPAlpha b ->
        case se of
          Alpha a ->
            if (a == b)
              then Just [] --Just ([(b, ESequence (Alpha a))])
              else Nothing
          otherwise -> Nothing
      SPSPSeq seqPattern1 seqPattern2 ->
        case se of
          Seq seq1 seq2 -> ((evalSequencePattern seq1 seqPattern1) ?++! (evalSequencePattern seq2 seqPattern2))
          Alpha a ->
            case seqPattern2 of
              SPSVar d -> Just [(d, (ESequence Nil))]
              otherwise -> Nothing
          otherwise -> Nothing
      SPSVar c -> Just [(c, (ESequence se))]
        -- case se of
        --   Seq sq -> Just [(s, (ESequence se))]
        --   otherwise -> Nothing
      SPVar c ->
        case se of
          Alpha a -> Just [(c, (ESequence (Alpha a)))]
          otherwise -> Nothing


  -- patternToSequencePattern :: Pattern -> SequencePattern
  -- patternToSequencePattern (PSequencePattern s) = s

  -- sequencePatternToPattern :: SequencePattern -> Pattern
  -- sequencePatternToPattern s = (PSequencePattern s)

  -- #############################################################################
  -- #############################################################################









  -- #############################################################################
  -- #### Printing utilities #####################################################

  -- putTerms :: [Term] ->



  -- Prints a Maybe term recursively in a readable way
  showMaybeTerm :: Maybe Term -> String
  showMaybeTerm t =
    case t of
      Just term -> showTerm term
      Nothing -> "-"


  -- Prints a term recursively in a readable way
  showTerm :: Term -> String
  showTerm t =
    case t of
      -- S (Sequence)
      TSequence sequence -> showSequence sequence

      -- (S)└ _| C (Looping and containment)
      LC sequence term -> "((" ++ (showSequence sequence) ++ ")└ ⎦ " ++ (showTerm term) ++ ")"

      -- T | T (Parallel Composition)
      PC termA termB -> "(" ++ (showTerm termA) ++ " | " ++ (showTerm termB) ++ ")"

      -- otherwise -> "Nothing"


  -- Prints a sequence recursively in a readable way
  showSequence :: Sequence -> String
  showSequence s =
    case s of
      -- a
      Alpha a -> "\x1b[33m" ++ (a : "\x1b[37m")

      -- S • S (Sequencing)
      Seq sequenceA sequenceB -> (showSequence sequenceA) ++ " • " ++ (showSequence sequenceB)

      otherwise -> "\x1b[30;1mNothing\x1b[37m"


  -- Prints a pattern recursively in a readable way
  showPattern :: Pattern -> String
  showPattern t =
    case t of
      -- S (Sequence)
      PSequencePattern sequence -> showSequencePattern sequence

      -- (S)└ _| C (Looping and containment)
      PLC sequence term -> "((" ++ (showSequencePattern sequence) ++ ")└ ⎦ " ++ (showPattern term) ++ ")"

      -- T | T (Parallel Composition)
      PPC termA termB -> "(" ++ (showPattern termA) ++ " | " ++ (showPattern termB) ++ ")"

      -- T (Term)
      PTerm term -> showTerm term

      PVar c -> "\x1b[31m" ++ (c : "\x1b[37m")


  -- Prints a sequence pattern in a readable way
  showSequencePattern :: SequencePattern -> String
  showSequencePattern s =
    case s of
      -- ε (Empty)
      SPNil -> ""

      -- a
      SPAlpha a -> "\x1b[33m" ++ (a : "\x1b[37m")

      -- S • S (Sequencing)
      SPSPSeq sequenceA sequenceB -> (showSequencePattern sequenceA) ++ " • " ++ (showSequencePattern sequenceB)

      -- S (Sequencing)
      SPSVar c -> "\x1b[31m" ++ (c : "\x1b[37m")

      SPVar c -> "\x1b[36m" ++ (c : "\x1b[37m")


  showExpr :: Expr -> String
  showExpr e = case e of
          ETerm term -> "(T)  " ++ (showTerm term)
          ESequence seq -> "(S)  " ++ (showSequence seq)


  showVars :: Maybe [(Char, Expr)] -> String
  showVars vars = case vars of
    Just vs -> "\n" ++ concat (map (\(c, e) -> "              " ++ (c : "") ++ " :  " ++ (showExpr e) ++ "\n") vs)
    Nothing -> "-"



  showRewrite :: Rewrite -> String
  showRewrite (Rewrite p1 p2) = (showPattern p1) ++ " --> " ++ (showPattern p2)

  -- #############################################################################
  -- #############################################################################

  -- Appends two Maybe lists; if one of them is Nothing, the other is returned.
  (?++) :: Maybe [a] -> Maybe [a] -> Maybe [a]
  (?++) a1 a2 =
    case a1 of
      Just ja1 ->
        case a2 of
          Just ja2 -> Just (ja1 ++ ja2)
          Nothing -> Just (ja1)
      Nothing ->
        case a2 of
          Just ja2 -> Just (ja2)
          Nothing -> Nothing

  -- Appends two Maybe lists; if one of them or both are Nothing, then Nothing
  -- is returned
  (?++!) :: Maybe [a] -> Maybe [a] -> Maybe [a]
  (?++!) a1 a2 =
    case a1 of
      Just ja1 ->
        case a2 of
          Just ja2 -> Just (ja1 ++ ja2)
          Nothing -> Nothing
      Nothing -> Nothing

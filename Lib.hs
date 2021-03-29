module Lib
    (
    Forest(Empty, Forest),
    SyntaxTree(Node),
    parse,
    tokenize,
    filterWS,
    formOutput
    ) where

import Data.List

--Terminals and NonTerminals
data NonTerm = G | T | T' | Q | Q' | Y
  deriving(Enum, Show, Eq)
data Term = EOF | TERM | NTERM | ARROW | EPSILON | OR | ENDL | AXIOM | COMMENT | EPS | ERR | S1 | S2 | S3 | S4 | S6
          | S7 | S8 | S9 | S10 | S12 | S14 | S15 | S16 | S17 | S21 | S23
  deriving(Enum, Show, Eq)

data Elem = Tr Term | NTr NonTerm
instance Show Elem where
  show (Tr s)  = show s
  show (NTr s) = show s

--Symbols
data Symbol = E | N | D | L | P | S | I | O | R | A | X | M | CH | AP | SG | NL | WS | DS | MR | QT | NM | EL
  deriving(Enum, Show)

--Position
data Position = Pos {line::Int, pos::Int, index::Int}
  deriving(Eq)
instance Show Position where
  show p = "(" ++ show (line p) ++ ", " ++ show (pos p) ++ ")"

--Fragment
data Fragment = Frag {from::Position, to::Position}
instance Show Fragment where
  show f = show (from f) ++ "--" ++ show (to f)

--Token
data Token = Token {tag::Term, frag::Fragment, v::String}
instance Show Token where
  show (Token EPS _ _) = show EPS
  show (Token t f "")  = show t ++ " " ++ show f
  show (Token t f v)   = show t ++ " " ++ show f ++ ": " ++ v


--Tree structure
data Forest = Empty
            | Forest [SyntaxTree]
  deriving(Show)

data SyntaxTree = Node {val::Token, children::Forest}
instance Show SyntaxTree where
  show st = "> " ++ treeShow "" st
    where
      treeShow :: String -> SyntaxTree -> String
      treeShow pref (Node v Empty) = show v
      treeShow pref (Node v (Forest ts)) = show v ++ showForest pref ts

      showForest :: String -> [SyntaxTree] -> String
      showForest pref [t] = "\n: " ++ showSon pref "\\--" "   " t
      showForest pref (t:ts) = "\n: " ++ showSon pref "|\\--" "|   " t ++ showForest pref ts

      showSon :: String -> String -> String -> SyntaxTree -> String
      showSon pref before next t = pref ++ before ++ treeShow (pref ++ next) t

--Tables
syntaxTable :: [[[Elem]]]
               {-         $         T                  NT                                           ->        eps           or                     endl      axm                 -}
syntaxTable = [{-G -}[[Tr ERR], [Tr ERR]         , [Tr ERR]                                   , [Tr ERR], [Tr ERR]    , [Tr ERR]             , [Tr ERR], [Tr AXIOM, NTr T', NTr T]],
               {-T -}[[Tr EPS], [Tr ERR]         , [NTr T', NTr T]                            , [Tr ERR], [Tr ERR]    , [Tr ERR]             , [Tr ERR], [Tr ERR]                 ],
               {-T'-}[[Tr ERR], [Tr ERR]         , [Tr NTERM, Tr ARROW, NTr Q, NTr Y, Tr ENDL], [Tr ERR], [Tr ERR]    , [Tr ERR]             , [Tr ERR], [Tr ERR]                 ],
               {-Q -}[[Tr ERR], [Tr TERM, NTr Q'], [Tr NTERM, NTr Q']                         , [Tr ERR], [Tr EPSILON], [Tr ERR]             , [Tr ERR], [Tr ERR]                 ],
               {-Q'-}[[Tr ERR], [Tr TERM, NTr Q'], [Tr NTERM, NTr Q']                         , [Tr ERR], [Tr ERR]    , [Tr EPS]             , [Tr EPS], [Tr ERR]                 ],
               {-Y -}[[Tr ERR], [Tr ERR]         , [Tr ERR]                                   , [Tr ERR], [Tr ERR]    , [Tr OR, NTr Q, NTr Y], [Tr EPS], [Tr ERR]                 ]]

axiom :: Elem
axiom = NTr G

lexTable :: [[Term]]
lexTable = [[NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, S1, COMMENT, EPS, EPS, S21, ERR, S23, ERR, ERR],
            [EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF],
            [NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, NTERM, EOF, EOF, EOF, EOF, EOF, EOF, EOF, NTERM, EOF],
            [EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF],
            [EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF],
            [EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF],
            [EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF],
            [EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF],
            [COMMENT, COMMENT, COMMENT, COMMENT, COMMENT, COMMENT, COMMENT, COMMENT, COMMENT, COMMENT, COMMENT , COMMENT,
             COMMENT, COMMENT, COMMENT, EOF, COMMENT, COMMENT, COMMENT, COMMENT, COMMENT, COMMENT],
            [EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EPS, EPS, EOF, EOF, EOF, EOF, EOF],
            [EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, EOF, ERR, EOF, ERR, ERR],
            [S2 , ERR, ERR, ERR, ERR, ERR, ERR, S12, ERR, S14, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, S3 , ERR, ERR, S6 , ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, S4 , ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ENDL, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ERR, ERR, S7 , ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ERR, ERR, ERR, S8 , ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, S9 , ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ERR, ERR, ERR, ERR, S10, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, EPSILON, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR , ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, OR , ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, S15, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ERR, ERR, ERR, S16, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ERR, ERR, ERR, ERR, S17, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, AXIOM, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR],
            [ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR, ARROW, ERR, ERR, ERR],
            [S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, S23, TERM, S23, S23]]

deltaS :: NonTerm -> Term -> [Elem]
deltaS nt t = col (fromEnum t) $ row (fromEnum nt) syntaxTable
  where
    row :: Int -> [[[Elem]]] -> [[Elem]]
    row 0 (r:rs) = r
    row n (r:rs) = row (n - 1) rs
    
    col :: Int -> [[Elem]] -> [Elem]
    col 0 (c:cs) = c
    col n (c:cs) = col (n - 1) cs

deltaL :: Term -> Symbol -> Term
deltaL t s = col (fromEnum s) $ row (fromEnum t) lexTable
  where
    row :: Int -> [[Term]] -> [Term]
    row 0 (r:rs) = r
    row n (r:rs) = row (n - 1) rs

    col :: Int -> [Term] -> Term
    col 0 (c:cs) = c
    col n (c:cs) = col (n - 1) cs

--Char -> Enum
charToSymbol :: Char -> Symbol
charToSymbol 'e' = E
charToSymbol 'n' = N
charToSymbol 'd' = D
charToSymbol 'l' = L
charToSymbol 'p' = P
charToSymbol 's' = S
charToSymbol 'i' = I
charToSymbol 'o' = O
charToSymbol 'r' = R
charToSymbol 'a' = A
charToSymbol 'x' = X
charToSymbol 'm' = M
charToSymbol '\'' = AP
charToSymbol '#' = SG
charToSymbol '\n' = NL
charToSymbol '-' = DS
charToSymbol '>' = MR
charToSymbol '"' = QT
charToSymbol  c
  | c `elem` ['0'..'9'] = NM
  | c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] = CH
  | c `elem` [' ', '\t'] = WS
  | otherwise = EL

--parse function
parse :: [Token] -> SyntaxTree
parse ts = tree $ growTree axiom ts
  where
    tree :: (SyntaxTree, [Token]) -> SyntaxTree
    tree (st, [Token EOF _ _]) = st
    tree (st, t:ts) = error $ "incorrect syntax " ++ show t

    growTree :: Elem -> [Token] -> (SyntaxTree, [Token])
    growTree (NTr s) (t:ts)  =
      let (trs, ts') = growForest (deltaS s $ tag t) (t:ts) []
      in (Node t $ Forest trs, ts')
    growTree (Tr EPS) ts     =
      (Node (Token EPS (Frag (Pos 0 0 (-1)) (Pos 0 0 (-1))) "") Empty, ts)
    growTree (Tr ERR) (t:ts) =
      error $ "incorrect syntax " ++ show t
    growTree (Tr s) (t:ts)
      | s == tag t = (Node t Empty, ts)
      | otherwise = error $ "incorrect syntax " ++ show t

    growForest :: [Elem] -> [Token] -> [SyntaxTree] -> ([SyntaxTree], [Token])
    growForest [] ts trs     =
      (trs, ts)
    growForest (s:ss) ts trs =
      let (tr, ts') = growTree s ts
      in growForest ss ts' (tr:trs)

--lexical analysis
tokenize :: String -> [Token]
tokenize = walkThrough (Pos 1 1 0) EOF
  where
    walkThrough :: Position -> Term -> String -> [Token]
    walkThrough pos _ []     = 
      [Token EOF (Frag pos pos) ""]
    walkThrough pos t (c:cs) =
      let (pos', t', val, cs') = formToken (nextPos pos c) (deltaL t $ charToSymbol c) [c] cs
      in Token t' (Frag pos pos') (valOf val) : walkThrough pos' EOF cs'
      
    formToken :: Position -> Term -> String -> String -> (Position, Term, String, String)
    formToken ps t v [] = 
      (ps, t, reverse v, [])
    formToken ps t v (c:cs)
      | deltaL t (charToSymbol c) /= EOF = formToken (nextPos ps c) (deltaL t $ charToSymbol c) (c:v) cs
      | otherwise                        = (ps, t, reverse v, c:cs)

    nextPos :: Position -> Char -> Position
    nextPos ps '\n' = Pos (line ps + 1) 1 (index ps + 1)
    nextPos ps _    = Pos (line ps) (pos ps + 1) (index ps + 1)
    
    valOf :: String -> String
    valOf "'axiom" = ""
    valOf "'endl" = ""
    valOf "'or" = ""
    valOf "'epsilon" = ""
    valOf "->" = ""
    valOf v   = v

--filter whitespace
filterWS :: [Token] -> [Token]
filterWS = filter (\t -> tag t /= EPS)

--formation of an output
formOutput :: String -> (SyntaxTree, [Token])
formOutput prog = formTree $ partition (\t -> tag t /= COMMENT && tag t /= ERR) (filterWS $ tokenize prog)
  where
    formTree :: ([Token],[Token]) -> (SyntaxTree, [Token])
    formTree (ts, errs) = (parse ts, errs)

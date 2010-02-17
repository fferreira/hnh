module SamplePrograms where


adderFunction =" \
\  adder :: Int -> Int \
\ adder a b = a + b"

funcWithLet ="\
\f x = let a = 1; b = 2 \n\
\          g y = exp2 \n\ 
\       in exp1"

layedOutFunc = "f x = let { a = 1; b = 2 ; g y = exp2 } in exp1"


-- declarations


sampleDeclarations :: [String]
sampleDeclarations = datatypeDeclarations ++ patternDeclarations

datatypeDeclarations = [ "type New = Old"
                       , "type A1 = A2 type A3 = A4"
                       , "data New = Old"
                       , "data A1 = A2 | A3"
                       , "data A1 = A2 A3 A4 A5 | A6 A7"
                       , "data A1 = A2 | A3 type A1 = A2 type A3 = A4 data A1 = A2 A3 A4 A5 | A6 A7" 
                       , "data A a = B a"
                       , "data A a b cd = B ab | C cd"
                       , "type A z = B z"
                       , "infix 6 +, -"
                       , "infixl **, +-"
                       , "infixr 2 `op`"
                       ]

patternDeclarations = [ "a = joker"
                      , "(a) = joker"
                      , "(a, b) = joker"
                      , "[a] = joker"
                      , "a:xs = joker"
                      , "Type _ a = joker"
                      ]
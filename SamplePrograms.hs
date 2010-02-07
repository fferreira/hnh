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
sampleDeclarations = [ "type New = Old"
                     , "type A1 = A2 type A3 = A4"
                     , "data New = Old"
                     , "data A1 = A2 | A3"
                     , "data A1 = A2 A3 A4 A5 | A6 A7"
                     , "data A1 = A2 | A3 type A1 = A2 type A3 = A4 data A1 = A2 A3 A4 A5 | A6 A7" 
                     {-
                     , "newtype A1 = A2"
                     , "newtype A1 = A2 newtype A3 = A4"
                     , "newtype A1 = A2 data New = Old"
                      -}
                     , "infix 6 +, -"
                     , "infixl **, +-"
                     , "infix 2 `op`"
                     ]
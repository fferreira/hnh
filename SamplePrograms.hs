module SamplePrograms
    (
     adderFunction,
     funcWithLet,
     layedOutFunc
    )
where


adderFunction =" \
\  adder :: Int -> Int \
\ adder a b = a + b"

funcWithLet ="\
\f x = let a = 1; b = 2 \n\
\          g y = exp2 \n\ 
\       in exp1"

layedOutFunc = "f x = let { a = 1; b = 2 ; g y = exp2 } in exp1"
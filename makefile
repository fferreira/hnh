# Simplistic makefile (can easily be improved, but suffices for now.
complete: compile

compile: Lexer.hs Parser.hs
	ghc --make Compiler

run: complete
	ghci Lexer.hs Parser.hs

Lexer.hs: Lexer.x
	alex -o Lexer.hs Lexer.x

Parser.hs: Parser.y
	happy -o Parser.hs Parser.y -i

clean:
	rm Lexer.hs Parser.hs *.hi *.o Parser.info

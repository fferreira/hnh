# Simplistic makefile (can easily be improved, but suffices for now.
complete: Lexer.hs Layout.hs Parser.hs Compiler.hs SamplePrograms.hs

compile: Lexer.hs Parser.hs
	ghc --make Compiler

run: complete
	ghci Lexer.hs Parser.hs

Lexer.hs: Lexer.x
	alex -o Lexer.hs Lexer.x

Parser.hs: Parser.y
	happy -o Parser.hs Parser.y

clean:
	rm Lexer.hs Parser.hs

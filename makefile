# Simplistic makefile (can easily be improved, but suffices for now.
comp: Lexer.hs

run: comp
	ghci Lexer.hs 

Lexer.hs: Lexer.x
	alex -o Lexer.hs Lexer.x

clean:
	rm Lexer.hs

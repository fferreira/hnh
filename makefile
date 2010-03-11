# Simplistic makefile (can easily be improved, but suffices for now.
complete: compile

compile: 
	ghc --make Main -o hnh

clean:
	rm *.hi *.o

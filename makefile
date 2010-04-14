# Simplistic makefile (can easily be improved, but suffices for now.
CFLAGS = -g 
complete: compile

runtime: runtime.h runtime.c code.c config.h

compile: 
	ghc --make Main -o hnh	
#	ghc -fwarn-unused-imports -fwarn-unused-binds --make Main -o hnh

clean:
	rm *.hi *.o

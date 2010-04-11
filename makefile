# Simplistic makefile (can easily be improved, but suffices for now.
complete: compile

runtime: runtime.h runtime.c

compile: 
	ghc --make Main -o hnh	
#	ghc -fwarn-unused-imports -fwarn-unused-binds --make Main -o hnh

clean:
	rm *.hi *.o

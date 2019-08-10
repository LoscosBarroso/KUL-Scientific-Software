hello:
	@gfortran -c ./hello_world.f95
	@gfortran -o ./hello hello_world.o
	@./hello
clean:
	@rm -f -r f_{files,modd}* *.o *.mod *.M *.d V*.inc *.vo \
	V*.f *.dbg album F.err

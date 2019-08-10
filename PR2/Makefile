hello_module.o:./hello_module.f95
	@gfortran -c ./hello_module.f95

hello_world:./hello_world.f95 hello_module.o
	@gfortran -c ./hello_world.f95
	@gfortran -o ./hello_world hello_world.o hello_module.o
	@./hello_world

rebels:./rebels.f95 
	@gfortran -c ./rebels.f95
	@gfortran -o ./rebels rebels.o 
	@./rebels

greycode:./greycode.f95 
	@gfortran -c ./greycode.f95
	@gfortran -o ./greycode greycode.o 
	@./greycode

doublef:./df.f95 
	@gfortran -c ./df.f95
	@gfortran -o doublef df.o 
	@./doublef

derf:./derf.f95 
	@gfortran -c ./derf.f95
	@gfortran -o derf derf.o 
	@./derf

quadratic:./quadratic.f90
	@gfortran -c ./quadratic.f90
	@gfortran -o quadratic quadratic.o 
	@./quadratic

clean:
	@rm -f -r f_{files,modd}* *.o *.mod *.M *.d V*.inc *.vo \
	V*.f *.dbg album F.err

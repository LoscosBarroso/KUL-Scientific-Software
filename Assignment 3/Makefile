matrixop.o:./matrixop.f90
	@gfortran -c ./matrixop.f90

drm:./dmr.f90 matrixop.o
	@gfortran -c  ./dmr.f90
	@gfortran -o  ./dmr dmr.o matrixop.o -llapack -lblas
plot:./plot.f90 matrixop.o
	@gfortran -c  ./plot.f90
	@gfortran -o  ./plot plot.o matrixop.o -llapack -lblas
clean:
	@rm *.mod
	@rm *.o
	@rm ./dmr

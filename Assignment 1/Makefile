inout.o:./inout.f90
	@gfortran -c ./inout.f90

matrix_solve.o:./matrix_solve.f90
	@gfortran -c ./matrix_solve.f90

main:./main.f90 inout.o matrix_solve.o 
	@gfortran -c ./main.f90
	@gfortran -o ./main main.o matrix_solve.o inout.o eigv_gfortran.o -llapack
	@./main


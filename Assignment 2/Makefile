inout2.o:./inout2.f90
	@gfortran -c ./inout2.f90

matrix_solve.o:./matrix_solve.f90
	@gfortran -c ./matrix_solve.f90

myeigv.o:./myeigv.f90
	@gfortran -c ./myeigv.f90

main:./main.f90 inout2.o matrix_solve.o myeigv.o
	@gfortran -c  ./main.f90
	@gfortran -o  ./main main.o matrix_solve.o inout2.o myeigv.o -llapack
clean:
	@rm *.mod
	@rm *.o
	@rm ./main


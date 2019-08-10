#!/bin/bash
# This script recompiles and runs the code with different optimization settings.
# The output of the executions is dumped to the results folder.
# This script does not change the working precision; this should be done at inout2.f90
make main
make clean
#No -Olevel
gfortran -c ./inout2.f90
gfortran -c ./matrix_solve.f90
gfortran -c ./myeigv.f90
gfortran -c ./main.f90
gfortran -o ./main main.o matrix_solve.o inout2.o myeigv.o -llapack
./main>outputs/Olevel0.txt
valgrind --log-file="outputs/valgrindO0.txt" ./main
make clean
#-Olevel = 1
gfortran -c -O1 ./inout2.f90
gfortran -c -O1 ./matrix_solve.f90
gfortran -c -O1 ./myeigv.f90
gfortran -c -O1 ./main.f90
gfortran -o ./main main.o matrix_solve.o inout2.o myeigv.o -llapack
./main>outputs/Olevel1.txt
valgrind --log-file="outputs/valgrindO1.txt" ./main
make clean
#-Olevel = 2
gfortran -c -O2 ./inout2.f90
gfortran -c -O2 ./matrix_solve.f90
gfortran -c -O2 ./myeigv.f90
gfortran -c -O2 ./main.f90
gfortran -o ./main main.o matrix_solve.o inout2.o myeigv.o -llapack
./main>outputs/Olevel2.txt
valgrind --log-file="outputs/valgrindO2.txt" ./main
make clean
#-Olevel = 3
gfortran -c -O3 ./inout2.f90
gfortran -c -O3 ./matrix_solve.f90
gfortran -c -O3 ./myeigv.f90
gfortran -c -O3 ./main.f90
gfortran -o ./main main.o matrix_solve.o inout2.o myeigv.o -llapack
./main>outputs/Olevel3.txt
valgrind --log-file="outputs/valgrindO3.txt" ./main
make clean
echo DONE

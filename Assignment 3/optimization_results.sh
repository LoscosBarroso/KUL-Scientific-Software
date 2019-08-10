#!/bin/bash
# This script recompiles and runs the code with different optimization settings.
# The output of the executions is dumped to the results folder.
# This script does not change the working precision; this should be done at inout2.f90
make main
make clean
#No -Olevel
gfortran -c ./matrixop.f90
gfortran -c ./dmr.f90
gfortran -o ./dmr dmr.o matrixop.o -llapack -lblas
echo now
./dmr>results/Olevel0N500B50.txt
#make clean
#-Olevel = 1
gfortran -c -O1 ./matrixop.f90
gfortran -c -O1 ./dmr.f90
gfortran -o -O1 ./dmr dmr.o matrixop.o -llapack -lblas
echo now
./dmr>results/Olevel1N500B50.txt
#make clean
#-Olevel = 2
gfortran -c -O2 ./matrixop.f90
gfortran -c -O2 ./dmr.f90
gfortran -o -O2 ./dmr dmr.o matrixop.o -llapack -lblas
echo now
./dmr>results/Olevel2N500B50.txt
#make clean
#-Olevel = 3
gfortran -c -O3 ./matrixop.f90
gfortran -c -O3 ./dmr.f90
gfortran -o -O3 ./dmr dmr.o matrixop.o -llapack -lblas
echo now
./dmr>results/Olevel3N500B50.txt
#make clean
echo DONE

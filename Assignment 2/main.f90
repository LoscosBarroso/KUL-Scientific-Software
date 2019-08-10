!Name: Daniel Loscos Barroso
!Compiler: gfortran
!Makefile included
!Answers: in output_dp.txt and output_sp.txt
program main

  use matrix_solve
  use myeigv
  use inout2
  implicit none
  real(prec), allocatable, dimension(:,:)  :: A ! input matrix
  real(prec), allocatable, dimension(:)  :: v,vp,x ! input vectors
  integer, allocatable, dimension(:)  :: per
  integer :: i,j,sizex,sizev,sizegg,sizehh
  integer, dimension(2) :: sizea
  character (len = 4) :: path
  integer(selected_int_kind(15)) :: ticks, rate
  real(selected_real_kind(15)) :: timeAtStart, timeAtEnd

  path='1234'
  !Testing everything still works 
  do j = 1, -1, 1 !skip test
  !do j = 1, 4, 1 !perform test
      call matrix_dims(sizea, ('test_matrices/A' // path(j:j)))
      call vector_length(sizex, ('test_matrices/x' // path(j:j)))
      call vector_length(sizev, ('test_matrices/v' // path(j:j)))
      allocate(A(sizea(1),sizea(2)),v(sizev),vp(sizev),x(sizex),per(sizea(1)))
      call  read_matrix(A, ('test_matrices/A' // path(j:j)))
      call  read_vector(v, ('test_matrices/v' // path(j:j)))
      vp = v
      call  read_vector(x, ('test_matrices/x' // path(j:j)))
      call gauss(A,v)
      print *, 'Gauss ', path(j:j),':'
      call read_matrix(A, ('test_matrices/A' // path(j:j)))
      print *,'  Absolute 2-norm of the backward error = ', norm2(vp-matmul(A,v))
      print *,'  Relative 2-norm of the backward error = ', norm2(vp-matmul(A,v))/norm2(vp)
      print *,'  2-norm of the forward error = ', norm2(v-x)
      call  read_vector(v, ('test_matrices/v' // path(j:j)))
      vp = v
      call  read_vector(x, ('test_matrices/x' // path(j:j)))
      call gauss_pivot(A,v,per)
      print *, 'Gauss Pivot ', path(j:j),':'
      call read_matrix(A, ('test_matrices/A' // path(j:j)))
      print *,'  Absolute 2-norm of the backward error = ', norm2(vp-matmul(A,v))
      print *,'  Relative 2-norm of the backward error = ', norm2(vp-matmul(A,v))/norm2(vp)
      print *,'  2-norm of the forward error = ', norm2(v-x)
      print *,'Eigenvalues:' 
      call eig(A,v,vp)
      do i = 1, sizev, 1
          print *, '(',v(i),',',vp(i),')'
      end do
      print *, 'Condition number = ',  cond(A) 
      print *, 'Sum of the eigenvalues = ', sum_ev(A)
      print *, ''
      deallocate(A,v,vp,x,per)
  end do

  !Actual stuff for this assignment
  do j = 1, 2, 1
      print *,'System Number ', path(j:j),':'
      !G dimensions are: sizea(2) x sizea(2)
      call matrix_dims(sizea, ('test_systems/A' // path(j:j)))
      call vector_length(sizegg, ('test_systems/g' // path(j:j)))
      call vector_length(sizehh, ('test_systems/h' // path(j:j)))
      !With this info we can build the full KKT matrix and store it in A
      allocate(A((sizea(2)+sizea(1)), (sizea(2)+sizea(1))),v((sizea(2)+sizea(1))), x((sizea(2)+sizea(1))))
      call read_vector(x(1:sizegg), ('test_systems/p' // path(j:j)))
      call read_vector(x(sizegg + 1:sizegg + sizehh), ('test_systems/lambda' // path(j:j)))
      A = 0
      call read_matrix(A(1:sizea(2),1:sizea(2)), ('test_systems/G' // path(j:j)))
      call read_matrix(A((sizea(2) + 1):(sizea(2) + sizea(1)),1:sizea(2)), ('test_systems/A' // path(j:j)))
      do i = 1, sizea(2), 1
         A(i,(sizea(2) + 1):) = A((sizea(2) + 1):,i)
      end do
      print *, 'Condition number of KKT matrix= ',  cond(A)  
      call read_vector(v(1:sizegg), ('test_systems/g' // path(j:j)))
      call read_vector(v(sizegg + 1:sizegg + sizehh), ('test_systems/h' // path(j:j)))
      !Now, we solve the system using gauss_pivot, we will store (g,h) in v and transform them into (x*,lamda*)
      print *, 'Gauss Pivot method:'
      allocate(per((sizea(2)+sizea(1))))
      call system_clock(count = ticks, count_rate = rate)
      timeAtStart = ticks !/ real(rate) ! Time in seconds
      call gauss_pivot(A,v,per)
      call system_clock(count = ticks, count_rate = rate)
      timeAtEnd = ticks !/ real(rate)
      deallocate(per)
      v(1:sizegg) = -v(1:sizegg)
      print *,'  Relative 2-norm of the forward error = ', norm2(v-x)/norm2(x)
      print *,'  Time taken by gauss pivot call is ', (timeAtEnd - timeAtStart)/real(rate) , 's.'
      deallocate (A)
      !Then we solve it using the schur complement method.
      print *, 'Schur complement method:'
      allocate(A((sizea(2)+sizea(1)), sizea(2)))
      A = 0
      call read_matrix(A(1:sizea(2),1:sizea(2)), ('test_systems/G' // path(j:j)))
      call read_matrix(A((sizea(2) + 1):(sizea(2) + sizea(1)),1:sizea(2)), ('test_systems/A' // path(j:j)))
      call read_vector(v(1:sizegg), ('test_systems/g' // path(j:j)))
      i = sizea(2) + sizea(1)
      call read_vector(v(sizegg + 1:i), ('test_systems/h' // path(j:j)))
      call system_clock(count = ticks, count_rate = rate)
      timeAtStart = ticks !/ real(rate) ! Time in seconds
      call schur(A(1:sizea(2),1:sizea(2)), A((sizea(2) + 1):i,1:sizea(2)), v(1:sizegg), v(sizegg + 1: i))
      call system_clock(count = ticks, count_rate = rate)
      timeAtEnd = ticks !/ real(rate)
      print *,'  Relative 2-norm of the forward error = ', norm2(v-x)/norm2(x)
      print *,'  Time taken by shur-comp.  call is ', (timeAtEnd - timeAtStart)/real(rate) , 's.'
      deallocate(v,x,A)
  end do


contains

    function sum_ev(A) result(sumev)
      real(prec), dimension(:,:), intent(in)  :: A ! input
      real(prec), dimension(size(A,1)) :: r,i
      real(prec) :: sumev !output
      call eig(A,r,i)
      sumev = sum(r)
    end function sum_ev

    function cond(A) result(condn)
      real(prec), dimension(:,:), intent(in)  :: A ! input
      real(prec),  dimension(size(A,1)) :: r,i
      real(prec) :: condn !output
      call eig(matmul(transpose(A), A),r,i)
      condn = sqrt(abs(maxval(r)/minval(r)))
    end function cond

end program

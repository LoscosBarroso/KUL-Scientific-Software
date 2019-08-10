!Name: Daniel Loscos Barroso
!Compiler: gfortran
!Makefile included
!Answers: in output_dp.txt and output_sp.txt
program main

  use matrix_solve
  use eigv_gfortran
  implicit none
  !integer, parameter :: pr = selected_real_kind(15)
  integer, parameter :: pr = selected_real_kind(5)
  real(pr), dimension(1:5,1:5)  :: A1 ! input matrix
  real(pr), dimension(1:5)  :: v1,vp1,x1 ! input vectors
  real(pr), dimension(1:11,1:11)  :: A2,A3 ! input matrix
  real(pr), dimension(1:11)  :: v2,vp2,x2,v3,vp3,x3 ! input vectors
  real(pr), dimension(1:14,1:14)  :: A4 ! input matrix
  real(pr), dimension(1:14)  :: v4,vp4,x4 ! input vectors
  integer :: i
  call  read_matrix(A1, 'test_matrices/A1')
  call  read_vector(v1, 'test_matrices/v1')
  vp1 = v1
  call  read_vector(x1, 'test_matrices/x1')
  call gauss(A1,v1,5)
  print *, 'Gauss 1:'
  call read_matrix(A1, 'test_matrices/A1')
  print *,'  Absolute 2-norm of the backward error = ', norm2(vp1-matmul(A1,v1))
  print *,'  Relative 2-norm of the backward error = ', norm2(vp1-matmul(A1,v1))/norm2(vp1)
  print *,'  2-norm of the forward error = ', norm2(v1-x1)
  call  read_vector(v1, 'test_matrices/v1')
  vp1 = v1
  call  read_vector(x1, 'test_matrices/x1')
  call gauss_pivot(A1,v1,5)
  print *, 'Gauss Pivot 1:'
  call read_matrix(A1, 'test_matrices/A1')
  print *,'  Absolute 2-norm of the backward error = ', norm2(vp1-matmul(A1,v1))
  print *,'  Relative 2-norm of the backward error = ', norm2(vp1-matmul(A1,v1))/norm2(vp1)
  print *,'  2-norm of the forward error = ', norm2(v1-x1)
  print *,'Eigenvalues:'
  call eig(A1,v1,vp1)
  do i = 1, 5, 1
      print *, '(',v1(i),',',vp1(i),')'
  end do
  print *, 'Condition number = ',  cond(A1,5) 
  print *, 'Sum of the eigenvalues = ', sum_ev(A1,5)
  print *, ''


  call  read_matrix(A2, 'test_matrices/A2')
  call  read_vector(v2, 'test_matrices/v2')
  vp2 = v2
  call  read_vector(x2, 'test_matrices/x2')
  call gauss(A2,v2,11)
  print *, 'Gauss 2:'
  call read_matrix(A2, 'test_matrices/A2')
  print *,'  Absolute 2-norm of the backward error = ', norm2(vp2-matmul(A2,v2))
  print *,'  Relative 2-norm of the backward error = ', norm2(vp2-matmul(A2,v2))/norm2(vp2)
  print *,'  2-norm of the forward error = ', norm2(v2-x2)
  call  read_vector(v2, 'test_matrices/v2')
  vp2 = v2
  call  read_vector(x2, 'test_matrices/x2')
  call gauss_pivot(A2,v2,11)
  print *, 'Gauss Pivot 2:'
  call read_matrix(A2, 'test_matrices/A2')
  print *,'  Absolute 2-norm of the backward error = ', norm2(vp2-matmul(A2,v2))
  print *,'  Relative 2-norm of the backward error = ', norm2(vp2-matmul(A2,v2))/norm2(vp2)
  print *,'  2-norm of the forward error = ', norm2(v2-x2)
  print *,'Eigenvalues:'
  call eig(A2,v2,vp2)
  do i = 1, 11, 1
      print *, '(',v2(i),',',vp2(i),')'
  end do
  print *, 'Condition number = ',  cond(A2,11) 
  print *, 'Sum of the eigenvalues = ', sum_ev(A2,11)
  print *, ''
  
  call  read_matrix(A3, 'test_matrices/A3')
  call  read_vector(v3, 'test_matrices/v3')
  vp3 = v3
  call  read_vector(x3, 'test_matrices/x3')
  call gauss(A3,v3,11)
  print *, 'Gauss 3:'
  call read_matrix(A3, 'test_matrices/A3')
  print *,'  Absolute 2-norm of the backward error = ', norm2(vp3-matmul(A3,v3))
  print *,'  Relative 2-norm of the backward error = ', norm2(vp3-matmul(A3,v3))/norm2(vp3)
  print *,'  2-norm of the forward error = ', norm2(v3-x3)
  call  read_vector(v3, 'test_matrices/v3')
  vp3 = v3
  call  read_vector(x3, 'test_matrices/x3')
  call gauss_pivot(A3,v3,11)
  print *, 'Gauss Pivot 3:'
  call read_matrix(A3, 'test_matrices/A3')
  print *,'  Absolute 2-norm of the backward error = ', norm2(vp3-matmul(A3,v3))
  print *,'  Relative 2-norm of the backward error = ', norm2(vp3-matmul(A3,v3))/norm2(vp3)
  print *,'  2-norm of the forward error = ', norm2(v3-x3)
  print *,'Eigenvalues:'
  call eig(A3,v3,vp3)
  do i = 1, 11, 1
      print *, '(',v3(i),',',vp3(i),')'
  end do
  print *, 'Condition number = ',  cond(A3,11) 
  print *, 'Sum of the eigenvalues = ', sum_ev(A3,11)
  print *, ''

  call  read_matrix(A4, 'test_matrices/A4')
  call  read_vector(v4, 'test_matrices/v4')
  vp4 = v4
  call  read_vector(x4, 'test_matrices/x4')
  call gauss(A4,v4,14)
  print *, 'Gauss 4:'
  call read_matrix(A4, 'test_matrices/A4')
  print *,'  Absolute 2-norm of the backward error = ', norm2(vp4-matmul(A4,v4))
  print *,'  Relative 2-norm of the backward error = ', norm2(vp4-matmul(A4,v4))/norm2(vp4)
  print *,'  2-norm of the forward error = ', norm2(v4-x4)
  call  read_vector(v4, 'test_matrices/v4')
  vp4 = v4
  call  read_vector(x4, 'test_matrices/x4')
  call gauss_pivot(A4,v4,14)
  print *, 'Gauss Pivot 4:'
  call read_matrix(A4, 'test_matrices/A4')
  print *,'  Absolute 2-norm of the backward error = ', norm2(vp4-matmul(A4,v4))
  print *,'  Relative 2-norm of the backward error = ', norm2(vp4-matmul(A4,v4))/norm2(vp4)
  print *,'  2-norm of the forward error = ', norm2(v4-x4)
  print *,'Eigenvalues:'
  call eig(A4,v4,vp4)
  do i = 1, 14, 1
      print *, '(',v4(i),',',vp4(i),')'
  end do
  print *, 'Condition number = ',  cond(A4,14) 
  print *, 'Sum of the eigenvalues = ', sum_ev(A4,14)
  print *, ''
  
contains

    function sum_ev(A,m) result(sumev)
      integer, intent(in) :: m
      real(pr), dimension(1:m,1:m), intent(in)  :: A ! input
      real(pr), dimension(1:m) :: r,i
      real(pr) :: sumev !output
      call eig(A,r,i)
      sumev = sum(r)
    end function sum_ev

    function cond(A,m) result(condn)
      integer, intent(in) :: m
      real(pr), dimension(1:m,1:m), intent(in)  :: A ! input
      real(pr), dimension(1:m) :: r,i
      real(pr) :: condn !output
      call eig(matmul(transpose(A), A),r,i)
      condn = sqrt(abs(maxval(r)/minval(r)))
    end function cond

end program

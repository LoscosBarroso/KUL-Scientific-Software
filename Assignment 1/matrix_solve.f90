!Name: Daniel Loscos Barroso
!Compiler: gfortran
!Makefile included
!Answers: in output_dp.txt and output_sp.txt
module matrix_solve

  use inout
  implicit none

contains

    subroutine gauss (A,v,m)
      integer, intent(in) :: m
      real(prec), dimension(1:m,1:m), intent(inout)  :: A ! input
      real(prec), dimension(1:m), intent(inout) :: v !input
      real(prec), dimension(1:m) :: y !input
      integer :: k,j

      do k = 1, m-1, 1
        do j = k+1, m, 1
          A(j,k) = A(j,k) / A(k,k)
          A(j,k+1:) = A(j,k+1:) - (A(k,k+1:)*A(j,k))
        end do
      end do
      !A = LU
      !L(j, :j-1) = A(j, :j-1)
      !U(j,j:   ) = A(j,j:   )
      !L*y = v
      do k = 1, m, 1
          y(k) = v(k)
          do j = 1, k-1, 1
            y(k) = y(k) - A(k,j)*y(j)
          end do
      end do
      !U*x = y, but now we call 'x' v
      do k = m, 1, -1
          v(k) = y(k)/A(k,k)
          do j = m, k+1, -1
            v(k) = v(k) - A(k,j)*v(j)/A(k,k)
          end do
      end do

    end subroutine gauss
 
    subroutine gauss_pivot(A,v,m)
      integer, intent(in) :: m
      real(prec), dimension(1:m,1:m), intent(inout)  :: A ! input
      real(prec), dimension(1:m), intent(inout) :: v !input
      real(prec), dimension(1:m) :: y !input
      !Lineal representation of the permutation matrix.
      integer :: k,j,pivot
      real(prec), dimension(1:m) :: p,aux
      
      p = [(k,k=1,m)] 
      do k = 1, m-1, 1
        !Index of the maximum (sub)diagonal value of column k.
        !The first k-1 values are above the diagonal, since maxloc doesn't
        !look here, we need to add this indexes to get the right pivot.
        pivot = maxloc(abs(A(p(k:),k)),1) + k-1
	if (k .ne. pivot) then
          p([pivot, k]) =  p([k, pivot])
        endif
        do j = k+1, m, 1
	  A(p(j),k) = A(p(j),k) / A(p(k),k)
          A(p(j),k+1:) = A(p(j),k+1:) - A(p(j),k)*A(p(k),k+1:)
        end do
      end do
      !PA = LU
      !L(i, :i-1) = A(p(i), :i-1)
      !U(i,i:   ) = A(p(i),i:   )
      !L*y = P*v
      do k = 1, m, 1
          y(k) = v(p(k))
          do j = 1, k-1, 1
            y(k) = y(k) - A(p(k),j)*y(j)
          end do
      end do
      !U*x = y, but now we call 'x' v
      do k = m, 1, -1
          v(k) = y(k)/A(p(k),k)
          do j = m, k+1, -1
            v(k) = v(k) - A(p(k),j)*v(j)/A(p(k),k)
          end do
      end do
    end subroutine gauss_pivot

endmodule matrix_solve
   

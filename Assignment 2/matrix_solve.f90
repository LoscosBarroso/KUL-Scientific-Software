!Name: Daniel Loscos Barroso
!Compiler: gfortran
!Makefile included
!Answers: in output_dp.txt and output_sp.txt
module matrix_solve

  use inout2
  implicit none

contains

    subroutine gauss (A,v)
      real(prec), dimension(:,:), intent(inout)  :: A ! input
      real(prec), dimension(:), intent(inout) :: v !input
      integer :: m,k,j
      m = size(A,1)
      do k = 1, m-1, 1
        do j = k+1, m, 1
          A(j,k) = A(j,k) / A(k,k)
          A(j,k+1:) = A(j,k+1:) - (A(k,k+1:)*A(j,k))
        end do
      end do
      call LUreescale(A,v)
    end subroutine gauss
 
    subroutine gauss_pivot(A,v,p)
      real(prec), dimension(:,:), intent(inout)  :: A ! input
      real(prec), dimension(:), intent(inout) :: v !input
      integer, dimension(:), intent(out) :: p !input
      !Lineal representation of the permutation matrix.
      integer :: m,k,j,pivot
      m =size(A,1)
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
      call LUreescalep(A,v,p)
    end subroutine gauss_pivot

    subroutine schur(G, A, gg, hh)
      real(prec), dimension(:,:), intent(inout)  :: G,A ! input
      real(prec), dimension(:), intent(inout) :: gg,hh !input
      real(prec), dimension(size(A,2),size(A,1)) :: GA
      integer, dimension(size(G,1)) :: p
      real(prec), dimension(size(gg)) :: ig
      integer, dimension(size(hh)) :: ph
      integer :: gdim,k,atcols 
      atcols = size(A,1)
      ig = gg
      !First we calculate G^(-1)g and the LU factorization of G
      !G^(-1)g is stored in ig and the LU factorization in G, with permutations saved in p.
      call gauss_pivot(G,ig,p)
      !Now, we will build G^(-1)tanspose(A) column by column using LU reescalations.
      !This will be stored in GA
      do k=1, atcols, 1
        GA(:,k) = A(k,:)
        call LUreescalep(G,GA(:,k),p)
      end do
      !To obtain lambda* we prepare and compute system (7)
      GA(1:size(GA,2),1:size(GA,2)) = matmul(A,GA)
      hh = matmul(A,ig) - hh
      !lambda* is stored in the no longer needed vector hh
      call gauss_pivot(GA(1:size(GA,2),:),hh,ph)
      !Finally we obtain p and store it in gg
      gg = matmul(transpose(A),hh) - gg
      call LUreescalep(G,gg,p)
    end subroutine schur

    subroutine LUreescale(LU,v)
      real(prec), dimension(:,:), intent(inout)  :: LU ! input
      real(prec), dimension(:), intent(inout) :: v !input
      real(prec), dimension(size(LU,1)) :: y
      integer :: m,k,j
      m =size(LU,1)
      !A = LU
      !L(i, :i-1) = LU(i, :i-1)
      !U(i,i:   ) = LU(i,i:   )
      !L*y = v
      do k = 1, m, 1
          y(k) = v(k)
          do j = 1, k-1, 1
            y(k) = y(k) - LU(k,j)*y(j)
          end do
      end do
      !U*x = y, but now we call 'x' v
      do k = m, 1, -1
          v(k) = y(k)/LU(k,k)
          do j = m, k+1, -1
            v(k) = v(k) - LU(k,j)*v(j)/LU(k,k)
          end do
      end do
    end subroutine LUreescale

    subroutine LUreescalep(LU,v,p)
      real(prec), dimension(:,:), intent(inout)  :: LU ! input
      real(prec), dimension(:), intent(inout) :: v !input
      integer, dimension(:), intent(in) :: p !input
      real(prec), dimension(size(LU,1)) :: y
      integer :: m,k,j
      m =size(LU,1)
      !PA = LU
      !L(i, :i-1) = LU(p(i), :i-1)
      !U(i,i:   ) = LU(p(i),i:   )
      !L*y = P*v
      do k = 1, m, 1
          y(k) = v(p(k))
          do j = 1, k-1, 1
            y(k) = y(k) - LU(p(k),j)*y(j)
          end do
      end do
      !U*x = y, but now we call 'x' v
      do k = m, 1, -1
          v(k) = y(k)/LU(p(k),k)
          do j = m, k+1, -1
            v(k) = v(k) - LU(p(k),j)*v(j)/LU(p(k),k)
          end do
      end do
    end subroutine LUreescalep

endmodule matrix_solve
   

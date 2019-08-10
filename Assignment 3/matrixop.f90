!   Implementation of several square matrix-matrix products

module matrixop
    implicit none
    integer, parameter :: dp = selected_real_kind(15,307)
contains
    !--------------------------------------------------------------------------
    ! 1. Three nested loops
    !
    ! NOTE: use the following convention for the indices
    !       i = row index of A
    !       j = column index of B
    !       k = column index of A
    !--------------------------------------------------------------------------
    subroutine a_maal_b_ijk(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,j,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do i = 1, dims(1), 1
           do j = 1, dims(1), 1
              do k = 1, dims(2), 1
                 c(i,j) = c(i,j) + a(i,k)*b(k,j)
              end do
           end do
        end do
    
    end subroutine a_maal_b_ijk

    subroutine a_maal_b_ikj(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,j,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do i = 1, dims(1), 1
           do k = 1, dims(2), 1
              do j = 1, dims(1), 1
                 c(i,j) = c(i,j) + a(i,k)*b(k,j)
              end do
           end do
        end do
    end subroutine a_maal_b_ikj

    subroutine a_maal_b_jik(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,j,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do j = 1, dims(1), 1
           do i = 1, dims(1), 1
              do k = 1, dims(2), 1
                 c(i,j) = c(i,j) + a(i,k)*b(k,j)
              end do
           end do
        end do
    end subroutine a_maal_b_jik

    subroutine a_maal_b_jki(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,j,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do j = 1, dims(1), 1
           do k = 1, dims(2), 1
              do i = 1, dims(1), 1
                 c(i,j) = c(i,j) + a(i,k)*b(k,j)
              end do
           end do
        end do
    end subroutine a_maal_b_jki

    subroutine a_maal_b_kij(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,j,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do k = 1, dims(2), 1
           do i = 1, dims(1), 1
              do j = 1, dims(1), 1
                 c(i,j) = c(i,j) + a(i,k)*b(k,j)
              end do
           end do
        end do
    end subroutine a_maal_b_kij

    subroutine a_maal_b_kji(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,j,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do k = 1, dims(2), 1
           do j = 1, dims(1), 1
              do i = 1, dims(1), 1
                 c(i,j) = c(i,j) + a(i,k)*b(k,j)
              end do
           end do
        end do
    end subroutine a_maal_b_kji
    !--------------------------------------------------------------------------
    ! 2. Two nested loops with vector operations
    !--------------------------------------------------------------------------
    subroutine a_maal_b_ikj_vect(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do i = 1, dims(1), 1
           do k = 1, dims(2), 1
              c(i,:) = c(i,:) + a(i,k)*b(k,:)
           end do
        end do
    end subroutine a_maal_b_ikj_vect

    subroutine a_maal_b_jki_vect(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: j,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do j = 1, dims(1), 1
           do k = 1, dims(2), 1
              c(:,j) = c(:,j) + a(:,k)*b(k,j)
           end do
        end do
    end subroutine a_maal_b_jki_vect

    subroutine a_maal_b_kij_vect(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do k = 1, dims(2), 1
           do i = 1, dims(1), 1
              c(i,:) = c(i,:) + a(i,k)*b(k,:)
           end do
        end do
    end subroutine a_maal_b_kij_vect

    subroutine a_maal_b_kji_vect(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: j,k
        integer, dimension(2) :: dims
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do k = 1, dims(2), 1
           do j = 1, dims(1), 1
              c(:,j) = c(:,j) + a(:,k)*b(k,j)
           end do
        end do
    end subroutine a_maal_b_kji_vect
    !--------------------------------------------------------------------------
    ! 3. Two nested loops with dot_product
    !--------------------------------------------------------------------------
    subroutine a_maal_b_ij_dot_product(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,j,dims
        dims= size(a,1)
        c = 0.0_dp ! TODO: complete this subroutine
        do i = 1, dims, 1
           do j = 1, dims, 1
               c(i,j) = dot_product(a(i,:),b(:,j))
           end do
        end do
    end subroutine a_maal_b_ij_dot_product

    subroutine a_maal_b_ji_dot_product(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer :: i,j,dims
        dims= size(a,1)
        c = 0.0_dp ! TODO: complete this subroutine
        do j = 1, dims, 1
           do i = 1, dims, 1
               c(i,j) = dot_product(a(i,:),b(:,j))
           end do
        end do
    end subroutine a_maal_b_ji_dot_product
    !--------------------------------------------------------------------------
    ! 4. Two nested loops with dot_product and explicit transpose of matrix A
    !--------------------------------------------------------------------------
    subroutine a_maal_b_transp_ij_dot_product(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        real(kind=dp), dimension(size(a,1),size(a,2))  :: ta
        integer :: i,j,dims
        dims= size(a,1)
        ta = transpose(a)
        c = 0.0_dp ! TODO: complete this subroutine
        do i = 1, dims, 1
           do j = 1, dims, 1
               c(i,j) = dot_product(ta(:,i),b(:,j))
           end do
        end do
    end subroutine a_maal_b_transp_ij_dot_product

    subroutine a_maal_b_transp_ji_dot_product(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        real(kind=dp), dimension(size(a,1),size(a,2))  :: ta
        integer :: i,j,dims
        dims= size(a,1)
        ta = transpose(a)
        c = 0.0_dp ! TODO: complete this subroutine
        do j = 1, dims, 1
           do i = 1, dims, 1
               c(i,j) = dot_product(ta(:,i),b(:,j))
           end do
        end do
    end subroutine a_maal_b_transp_ji_dot_product
    !--------------------------------------------------------------------------
    ! 5. Using BLAS : Add library in linking phase
    !--------------------------------------------------------------------------
    subroutine a_maal_b_blas(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        c = 0.0_dp ! TODO: complete this subroutine
        call dgemm ('N', 'N', size(a,1), size(b,2), size(a,2), 1.0_dp, A, size(a,1), B, size(b,2), 0.0_dp, C, size(a,1))
    end subroutine a_maal_b_blas
   
    !--------------------------------------------------------------------------
    ! 6. In blocks
    !--------------------------------------------------------------------------
    subroutine a_maal_b_blocks(a, b, c, blocksize)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer, intent(in) :: blocksize
        real(kind=dp), dimension(blocksize,blocksize):: d
        integer :: bl,i,j,k
        integer, dimension(2) :: dims
        bl = blocksize - 1
        dims(1) = size(a,1)
        dims(2) = size(a,2)
        c = 0.0_dp ! TODO: complete this subroutine
        do j = 1, dims(1), blocksize
           do k = 1, dims(2), blocksize
              do i = 1, dims(1), blocksize
                 call a_maal_b_transp_ji_dot_product(a(i:(i+bl),k:(k+bl)), b(k:(k+bl),j:(j+bl)), d)
                 c(i:(i+bl),j:(j+bl)) = c(i:(i+bl),j:(j+bl)) + d
              end do
           end do
        end do
    end subroutine a_maal_b_blocks
    !--------------------------------------------------------------------------
    ! 7. Intrinsic matmul function
    !--------------------------------------------------------------------------
    subroutine a_maal_b_matmul(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        c = matmul( a, b ) ! Already completed
    end subroutine a_maal_b_matmul
    !--------------------------------------------------------------------------
    ! 8. Own variant
    ! Strassen algorithm
    !--------------------------------------------------------------------------
    subroutine own_variant(a, b, c)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        real(kind=dp), allocatable, dimension(:,:) :: sa, sb, sc
        integer :: newsize
        newsize = 2**(ceiling(log(size(a,1)*1.) / log(2.)))
        allocate (sa(newsize,newsize),sb(newsize,newsize),sc(newsize,newsize))
        c = 0.0_dp ! TODO: complete this subroutine
        sa = 0.0_dp
        sa(1:size(a,1),1:size(a,2))=a
        sb = 0.0_dp
        sb(1:size(b,1),1:size(b,2))=b
        sc = 0.0_dp
        call strass(sa,sb,sc,newsize/2)
        c = sc(1:size(c,1),1:size(c,2))
        deallocate (sa,sb,sc)
    end subroutine own_variant

recursive subroutine strass(a,b,c,bl)
        real(kind=dp), dimension(:,:), intent(in)  :: a, b
        real(kind=dp), dimension(:,:), intent(out) :: c
        integer, intent(in) :: bl
        integer :: l
        !check test.f90 to see why this is the best way to store the 7 M matrixes
        real(kind=dp), dimension(bl,bl,7) :: m
        if (bl .eq. 1) then
           m(1,1,1)= (a(1,1)+a(2,2))*(b(1,1)+b(2,2))
           m(1,1,2)= (a(2,1)+a(2,2))*b(1,1)
           m(1,1,3)= a(1,1)*(b(1,2)-b(2,2))
           m(1,1,4)= a(2,2)*(b(2,1)-b(1,1))
           m(1,1,5)= (a(1,1)+a(1,2))*b(2,2)
           m(1,1,6)= (a(2,1)-a(1,1))*(b(1,1)+b(1,2))
           m(1,1,7)= (a(1,2)-a(2,2))*(b(2,1)+b(2,2))
           c(1,1) = m(1,1,1) + m(1,1,4) - m(1,1,5) + m(1,1,7)
           c(1,2) = m(1,1,3) + m(1,1,5)
           c(2,1) = m(1,1,2) + m(1,1,4)
           c(2,2) = m(1,1,1) - m(1,1,2) + m(1,1,3) + m(1,1,6)
        else
           l = 2*bl
           call strass((a(1:bl,1:bl)+a(bl+1:l,bl+1:l)),(b(1:bl,1:bl)+b(bl+1:l,bl+1:l)),m(:,:,1),bl/2)
           call strass((a(bl+1:l,1:bl)+a(bl+1:l,bl+1:l)),b(1:bl,1:bl),m(:,:,2),bl/2)
           call strass(a(1:bl,1:bl),(b(1:bl,bl+1:l)-b(bl+1:l,bl+1:l)),m(:,:,3),bl/2)
           call strass(a(bl+1:l,bl+1:l),(b(bl+1:l,1:bl)-b(1:bl,1:bl)),m(:,:,4),bl/2)
           call strass((a(1:bl,1:bl)+a(1:bl,bl+1:l)),b(bl+1:l,bl+1:l),m(:,:,5),bl/2)
           call strass((a(bl+1:l,1:bl)-a(1:bl,1:bl)),(b(1:bl,1:bl)+b(1:bl,bl+1:l)),m(:,:,6),bl/2)
           call strass((a(1:bl,bl+1:l)-a(bl+1:l,bl+1:l)),(b(bl+1:l,1:bl)+b(bl+1:l,bl+1:l)),m(:,:,7),bl/2)
           c(1:bl,1:bl) = m(:,:,1) + m(:,:,4) - m(:,:,5) + m(:,:,7)
           c(1:bl,bl+1:l) = m(:,:,3) + m(:,:,5)
           c(bl+1:l,1:bl) = m(:,:,2) + m(:,:,4)
           c(bl+1:l,bl+1:l) = m(:,:,1) - m(:,:,2) + m(:,:,3) + m(:,:,6)
        endif
    end subroutine strass

end module matrixop

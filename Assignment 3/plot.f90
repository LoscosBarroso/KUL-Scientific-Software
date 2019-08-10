!   Tests of several square matrix-matrix products

program dmr
    use matrixop
    implicit none
    !--------------------------------------------------------------------------
    ! Abstract interfaces
    !
    ! NOTE: this simplifies the timings.
    !--------------------------------------------------------------------------
    abstract interface
        subroutine a_maal_b_interface(a, b, c)
            import dp
            real(kind=dp), dimension(:,:), intent(in)  :: a, b
            real(kind=dp), dimension(:,:), intent(out) :: c
        end subroutine a_maal_b_interface
        subroutine a_maal_b_blocks_interface(a,b,c,blocksize)
            import dp
            real(kind=dp), dimension(:,:), intent(in)  :: a, b
            real(kind=dp), dimension(:,:), intent(out) :: c
            integer, intent(in) :: blocksize
        end subroutine a_maal_b_blocks_interface
    end interface
    
    !--------------------------------------------------------------------------
    ! Main timing program
    !--------------------------------------------------------------------------
    integer :: k, N, blocksize
    real :: flops
    integer, dimension(:), allocatable :: seed
    real(kind=dp), dimension(:,:), allocatable :: a, b, c
    real(kind=dp), dimension(:,:), allocatable :: c_matmul
    do n=22,880,22
       print *, n
       blocksize = 22

    ! Make sure we use the same pseudo-random numbers each time by initializing
    ! the seed to a certain value.
    call random_seed(size=k)
    allocate(seed(k))
    seed = N
    call random_seed(put=seed)
   

    ! Allocate the matrices and one reference matrix
    allocate(a(N,N), b(N,N), c(N,N), c_matmul(N,N))
    call random_number(a)
    call random_number(b)
    call a_maal_b_matmul(a,b,c_matmul) ! Reference value

if (1==1) then    
    ! 1. Three nested loops
    call do_timing( "JKI", a_maal_b_jki )
    ! 2. Two nested loops with vector operations
    ! 3. Two nested loops with dot_product
    ! 4. Two nested loops with dot_product and explicit transpose of matrix A
    call do_timing( "JI TP DOT_PRODUCT", a_maal_b_transp_ji_dot_product )
    
    ! 5. Using BLAS
    call do_timing( "BLAS DGEMM", a_maal_b_blas )
end if    
    ! 6. In blocks
    call do_timing( "IN BLOCKS", method_blocks=a_maal_b_blocks )
    
    ! 7. Intrinsic matmul function
    call do_timing( "MATMUL", a_maal_b_matmul )
    ! 8. Own variant
    ! Clean up
    deallocate(a, b, c, c_matmul)
    deallocate(seed)
    end do
contains

    subroutine do_timing( name, method, method_blocks )
        character(len=*), intent(in) :: name
        procedure(a_maal_b_interface), optional :: method
        procedure(a_maal_b_blocks_interface), optional :: method_blocks
        real :: t1, t2, t3, t4
        ! Do the timing
        if( present(method) ) then
            call cpu_time(t1)
            call method( a, b, c )
            call cpu_time(t2)
            call method( a, b, c )
            call cpu_time(t3)
            call method( a, b, c )
            call cpu_time(t4)
        else
            call cpu_time(t1)
            call method_blocks( a, b, c, blocksize)
            call cpu_time(t2)
            call method_blocks( a, b, c, blocksize)
            call cpu_time(t3)
            call method_blocks( a, b, c, blocksize)
            call cpu_time(t4)
        end if
        t4 = t4 - t3
        t3 = t3 - t2
        t2 = t2 - t1

	print "(A, ES9.2, A, ES9.2, A, ES9.2, A, ES9.2)", " ", t2, " ", t3, " ", t4

    end subroutine do_timing

end program dmr

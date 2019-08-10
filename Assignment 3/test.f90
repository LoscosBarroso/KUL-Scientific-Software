!   Tests of how tridimensional matrixes ar stored

program dmr
    implicit none
    integer, dimension(7,2,2) :: m
    integer, dimension(2,2,7):: n
    integer :: i,j,k
    do i=1,7,1
    do j=1,2,1
    do k=1,2,1
    m(i,j,k)=(100*i)+(10*j)+k
    n(j,k,i)=(100*i)+(10*j)+k
    end do
    end do
    end do
    print *,m
    print *,n
contains

end program dmr

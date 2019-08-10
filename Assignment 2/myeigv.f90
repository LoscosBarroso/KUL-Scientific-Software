!Name: Daniel Loscos Barroso
!Compiler: gfortran
!Makefile included
!Answers: in output_dp.txt and output_sp.txt
module myeigv
  use inout2
  implicit none
    integer,parameter :: d = selected_real_kind(15)
    integer,parameter :: s = selected_real_kind(6)
  public eig

  interface eig
    module procedure seig
    module procedure deig
  end interface

contains

    subroutine seig(A,v,vp)
      real(s), dimension(:,:), intent(in)  :: A ! input
      real(s), dimension(:), intent(out) :: v,vp !input
      real(s) :: unused(1,1)
      integer :: info,lwork
      real(s), dimension((size(A(1,:)) *3)) :: work
      lwork = (size(A(1,:)) *3)
      call sgeev('N', 'N', size(A(1,:)), A, size(A(1,:)), v, vp, unused, 1, unused, 1, work, lwork, info)
    end subroutine seig

    subroutine deig(A,v,vp)
      real(d), dimension(:,:), intent(in)  :: A ! input
      real(d), dimension(:), intent(out) :: v,vp !input
      real(d) :: unused(1,1)
      integer :: info,lwork
      real(d), dimension((size(A(1,:)) *3)) :: work
      lwork = (size(A(1,:)) *3)
      call dgeev('N', 'N', size(A(1,:)), A, size(A(1,:)), v, vp, unused, 1, unused, 1, work, lwork, info)
    end subroutine deig

endmodule myeigv
   

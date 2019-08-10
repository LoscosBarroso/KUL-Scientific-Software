

program main
   implicit none

   integer, parameter :: dp = selected_real_kind(15)
   integer, parameter :: sp = selected_real_kind(6)
  
   real(dp) :: b,c
   read *,  b
   read *,  c
	    call alg_1_sp(real(b),real(c))
  	    call alg_2_sp(real(b),real(c))
	    call alg_1_dp(b,c)
	    call alg_2_dp(b,c)

contains

  subroutine alg_1_dp(b, c)
    real(dp), intent(in) :: b, c
    real(dp)             :: D

    D = sqrt((b/2.0_dp)**2 - c)
    print *,'alg1dp=', -b/2.0_dp + D, -b/2.0_dp -D
  end subroutine alg_1_dp

  
  subroutine alg_2_dp(b, c)
    real(dp), intent(in) :: b, c
    real(dp)             :: D, x_1

    D = sqrt((b/2.0_dp)**2 - c)
    x_1 = sign(abs(b/2.0_dp) + D, -b)
    print *,'alg2dp=', x_1, c/x_1
  end subroutine alg_2_dp

  
  subroutine alg_1_sp(b, c)
    real(sp), intent(in) :: b, c
    real(sp)             :: D

    D = sqrt((b/2)**2 - c)
    print *,'alg1sp=', -b/2 + D, -b/2 -D
  end subroutine alg_1_sp
  
  subroutine alg_2_sp(b, c)
    real(sp), intent(in) :: b, c
    real(sp)             :: D, x_1

    D = sqrt((b/2)**2 - c)
    x_1 = sign(abs(b/2) + D, -b)
    print *,'alg2sp=', x_1, c/x_1
  end subroutine alg_2_sp

end program main

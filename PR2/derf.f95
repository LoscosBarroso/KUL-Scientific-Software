   !Name: Daniel Loscos Barroso
   !Compiler:
   !Makefile included
   !Answers:


   PROGRAM derivf
      IMPLICIT NONE
      real, parameter :: alpha = 3.0
      integer, parameter :: sp = KIND(1.0)
      integer, parameter :: wp = SELECTED_REAL_KIND(2*precision(1.0_sp))
      real(kind = sp) :: h1,x1
      real(kind = wp) :: h,eps,x
      x1 = 1.0
      h1 = 1.0
      x = 1.0
      h = 1.0
      eps = 1.0E-016_wp
      DO WHILE (h > eps)
         PRINT *, h, abs(df1(x1)-approx_df1(x1,h1)), abs(df2(x)-approx_df2(x,h))
	 h = h/10.0_wp
         h1 = h1/10.0_sp
      END DO

   CONTAINS
      FUNCTION f1(x) RESULT (res)
	 real(sp) :: x,res
	 res = 1.0_sp/((x-alpha)**7)
      END FUNCTION f1

      FUNCTION approx_df1(x,h) RESULT (res)
	 real(sp) :: x,h,res
	 res = (f1(x+h)-f1(x-h))/(2.0_sp*h)
      END FUNCTION approx_df1

      FUNCTION df1(x) RESULT (res)
	 real(sp) :: x,res
	 res = -7.0_sp/((x-alpha)**8)
      END FUNCTION df1

      FUNCTION f2(x) RESULT (res)
	 real(wp) :: x,res
	 res = 1.0_wp/((x-alpha)**7)
      END FUNCTION f2

      FUNCTION approx_df2(x,h) RESULT (res)
	 real(wp) :: x,h,res
	 res = (f2(x+h)-f2(x-h))/(2.0_wp*h)
      END FUNCTION approx_df2

      FUNCTION df2(x) RESULT (res)
	 real(wp) :: x,res
	 res = -7.0_wp/((x-alpha)**8)
      END FUNCTION df2
   END PROGRAM derivf

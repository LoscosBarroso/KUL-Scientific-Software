   !Name: Daniel Loscos Barroso
   !Compiler:
   !Makefile included
   !Answers:


   PROGRAM doublef
      IMPLICIT NONE
      integer, parameter :: r4 = selected_int_kind(8) 
      integer, parameter :: r8 = selected_int_kind(15)
      INTEGER(kind = r4) :: N1
      INTEGER(kind = r8) :: N2
      N1 = -2
      N2 = -2
      DO WHILE (N1 <30)
          PRINT *,N1, df4(N1)
          N1 = N1+1
      END DO
      DO WHILE (N2 <30)
          PRINT *,N2, df8(N2)
          N2 = N2+1
      END DO

   CONTAINS
     RECURSIVE FUNCTION df4(N) RESULT (res)
       IMPLICIT NONE
       INTEGER res, N
       SELECT CASE (N)
	   CASE ( 1 : )
	      res = N * df4(N-2)  
	   CASE (-1:0)
	      res = 1
	   CASE DEFAULT
	      res = 0
	END SELECT
      END FUNCTION DF4
      
      RECURSIVE FUNCTION df8(N) RESULT (res)
       IMPLICIT NONE
       INTEGER(r8) res, N
       SELECT CASE (N)
	   CASE ( 1 : )
	      res = N * df8(N-2) 
	   CASE (-1:0)
	      res = 1
	   CASE DEFAULT
	      res = 0
	END SELECT
      END FUNCTION DF8
   END PROGRAM doublef

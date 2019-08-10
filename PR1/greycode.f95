   !Name: Daniel Loscos Barroso
   !Compiler:
   !Makefile included
   !Answers:

program grey
implicit none
   integer :: n
   integer :: i
   i = 1
   do while (i < 21)
      n = grey_fun(i)
      print '(i4)',n
      i = i+1
   enddo

contains
   function grey_fun(n) result (output)
      integer, intent(in) :: n
      integer output
      output = ISHFT (n,-1)
      output = IEOR (n,output)
   end function grey_fun

end program grey

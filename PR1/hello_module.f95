   !Name: Daniel Loscos Barroso
   !Compiler:
   !Makefile included
   !Answers:
module hello_module
   implicit none
   integer,public :: n
   integer :: iostat
   public :: hello_sub
contains
   subroutine hello_sub
      
      read( *, '( i20 )', iostat = iostat ) n
      if( iostat == 0 .and. n > 0 .and. n < 101) then
         do while  ( n .gt. 0 )
            print *, "Hello, world!"
            n = n-1
         enddo
      else
         write( *, * ) 'That was not an input between 1 and 100.'
      end if
   end subroutine hello_sub
endmodule hello_module
   

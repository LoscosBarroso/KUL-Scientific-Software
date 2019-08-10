module inout2
  
  implicit none
  integer, parameter :: prec = selected_real_kind(15) !Change precision here
  !integer, parameter :: prec = selected_real_kind(6) !Change precision here
  integer, parameter :: input_unit = 7

contains

  subroutine matrix_dims(dims, path)
    integer, dimension(2), intent(out) :: dims
    character(len=*), intent(in) :: path

    logical :: file_exists
    integer :: stat

    inquire(file=path, exist=file_exists)
    if(.not. file_exists) call ioCrash(path)
    open(input_unit, file=path)

    read(input_unit, *, iostat=stat) dims
    if(stat/=0) call ioCrash(path)

    close(input_unit)
  end subroutine matrix_dims

  subroutine vector_length(length, path)
    integer, intent(out) :: length
    character(len=*), intent(in) :: path

    logical :: file_exists
    integer :: stat

    inquire(file=path, exist=file_exists)
    if(.not. file_exists) call ioCrash(path)
    open(input_unit, file=path)

    read(input_unit, *, iostat=stat) length
    if(stat/=0) call ioCrash(path)

    close(input_unit)
  end subroutine vector_length
  
  subroutine read_matrix(A, path)
    real(prec), dimension(:,:), intent(inout) :: A
    character(len=*), intent(in) :: path

    logical :: file_exists
    integer, dimension(2) :: dims
    integer :: stat

    inquire(file=path, exist=file_exists)
    if(.not. file_exists) call ioCrash(path)
    open(input_unit, file=path)

    read(input_unit, *, iostat=stat) dims
    if(stat/=0) call ioCrash(path)
    if(dims(1)/=size(A,1) .or. dims(2)/=size(A,2)) then
       write(0,*) 'Invalid array size!'
       call ioCrash(path)
    end if
    read(input_unit, *) A
    if(stat/=0) call ioCrash(path)

    close(input_unit)
  end subroutine read_matrix

  subroutine read_vector(v, path)
    real(prec), dimension(:), intent(inout) :: v
    character(len=*), intent(in) :: path

    logical :: file_exists
    integer :: dim
    integer :: stat

    inquire(file=path, exist=file_exists)
    if(.not. file_exists) call ioCrash(path)
    open(input_unit, file=path)

    read(input_unit, *, iostat=stat) dim
    if(stat/=0) call ioCrash(path)
    if(dim/=size(v)) then
       write(0,*) 'Invalid array size!'
       call ioCrash(path)
    end if
    read(input_unit, *) v
    if(stat/=0) call ioCrash(path)

    close(input_unit)
  end subroutine read_vector

  subroutine ioCrash(path)
    character(len=*), optional, intent(in) :: path

    write(0,*) 'Encountered an I/O error, stopping execution!'
    write(0,*) 'Please check your input...'
    write(0,*) 'The following file was being read: ', path
    stop 1
  end subroutine ioCrash
end module

module module_file

use, intrinsic :: iso_c_binding
implicit none

private
public :: find_indexed

!> https://degenerateconic.com/fortran-c-interoperability.html
interface
  function popen(command, mode) bind(C, name='popen')
    import :: c_char, c_ptr
    character(kind=c_char) :: command(*)
    character(kind=c_char) :: mode(*)
    type(c_ptr) :: popen
  end function popen

  function fgets(s, siz, stream) bind(C, name='fgets')
    import :: c_char, c_ptr, c_int
    type(c_ptr) :: fgets
    character(kind=c_char) :: s(*)
    integer(kind=c_int), value :: siz
    type(c_ptr), value :: stream
  end function fgets

  function pclose(stream) bind(C, name='pclose')
    import :: c_ptr, c_int
    integer(c_int) :: pclose
    type(c_ptr), value :: stream
  end function pclose
end interface

contains

function c2f_string(c) result(f)
  character(len=*), intent(in) :: c
  character(len=:), allocatable :: f
  integer :: i

  i = index(c, c_null_char)
  if (i <= 0) then
    f = c
  else if (i == 1) then
    f = ''
  else if (i > 1) then
    f = c(1:i - 1)
  end if
end function c2f_string

function get_command(command) result(str)
  character(len=*), intent(in) :: command
  character(len=:), allocatable :: str
  integer, parameter :: buffer_length = 1000
  type(c_ptr) :: h
  integer(c_int) :: istat
  character(kind=c_char, len=buffer_length) :: line

  str = ''
  h = c_null_ptr
  h = popen(command//c_null_char, 'r'//c_null_char)

  if (c_associated(h)) then
    do while (c_associated(fgets(line, buffer_length, h)))
      str = str//c2f_string(line)
    end do
    istat = pclose(h)
  end if
end function get_command

!> Powershell also supports "ls"
function find_indexed(filepath, option) result(solved)
  character(len=*), intent(in) :: filepath, option
  integer, allocatable :: solved(:)
  integer :: loc, n, i
  character(len=:), allocatable :: str, tmp

  tmp = filepath//"/"//option//"_????.???"
  str = get_command("ls "//tmp)
  n = len(tmp)
  allocate (solved(len(str)/(n + 1)))

  deallocate (tmp)
  allocate (character(len=n) :: tmp)
  do i = 1, size(solved)
    loc = (i - 1)*(n + 1) + 1
    tmp = str(loc:loc + n - 1)
    read (tmp(n - 7:n - 4), "(i4.4)") solved(i)
  end do
end function find_indexed

end module module_file
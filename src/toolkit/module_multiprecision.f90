module module_multiprecision

use, intrinsic :: iso_fortran_env, only: int64
use :: module_utility, only: carry
implicit none

public :: long_type
public :: assignment(=)
public :: operator(+)
private

!> Long integer type
type :: long_type(len)
  integer, len :: len
  character :: sign = "+"
  integer :: start = 0
  integer(kind=int64) :: digit(len)
end type long_type

!> Assignment
interface assignment(=)
  module procedure :: new_long
end interface assignment(=)

!> Addition
interface operator(+)
  module procedure :: add
end interface operator(+)

contains

!> Constructor
pure subroutine new_long(long, array)
  type(long_type(len=*)), intent(inout) :: long
  integer(int64), intent(in) :: array(:)
  integer(int64) :: n, i

  long%sign = "+"
  if (long%len < size(array)) error stop &
    & "[new_long] Not enough memory."
  if (size(array) == 0) then
    long%start = 0
    long%digit = 0
  else if (size(array) == 1) then
    long%start = 1
    long%digit(:long%len - 1) = 0
    long%digit(long%len) = array(1)
  else
    do i = 1, size(array)
      if (array(i) /= 0) exit
    end do

    if (i == size(array) + 1) then
      long%digit = 0
    else if (i /= 0) then
      long%start = long%len - size(array(i:)) + 1
      long%digit(1:long%start - 1) = 0
      long%digit(long%start:long%len) = array(i:)
    end if
  end if
end subroutine new_long

!> inner function
pure subroutine addition_(ret, digit1, digit2)
  integer(int64), contiguous, intent(inout) :: ret(:)
  integer(int64), contiguous, intent(in) :: digit1(:), digit2(:)

  if (.not. (size(digit1) == size(digit2) .and. &
    & size(digit2) == size(ret))) error stop &
    & "[addition_] Invalid digit size."
  ret = digit1 + digit2
  call carry(ret)
end subroutine addition_

!> Addition function
pure function add(value1, value2) result(ret)
  type(long_type(len=*)), intent(in) :: value1, value2
  type(long_type(len=:)), allocatable :: ret
  integer(int64) :: digit(value1%len) 

  if (allocated(ret)) then
    if (ret%len /= value1%len) then
      deallocate (ret)
      allocate (long_type(len=value1%len) :: ret)
    end if
  else
    allocate (long_type(len=value1%len) :: ret)
  end if

  if (value1%sign == value2%sign) then
    call addition_(digit, value1%digit, value2%digit)
    call new_long(ret, digit)
  else
    error stop "[add] Not supported yet."
  end if
end function add

end module module_multiprecision
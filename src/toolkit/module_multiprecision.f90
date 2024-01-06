module module_multiprecision

use, intrinsic :: iso_fortran_env, only: int64
use :: module_utility, only: carry, to_integer, to_array
implicit none

public :: long_type
public :: assignment(=)
public :: operator(+)
public :: operator(*)
public :: operator(**)
public :: size
private

!> Long integer type
type :: long_type(len)
  integer(int64), len :: len
  integer(int64) :: start = 0
  character :: sign = "+"
  integer(int64) :: digit(len)
end type long_type

!> Assignment
interface assignment(=)
  module procedure :: assign
end interface assignment(=)

!> Addition
interface operator(+)
  module procedure :: add
end interface operator(+)

!> Multiplication
interface operator(*)
  module procedure :: multiply
end interface operator(*)

!> Power
interface operator(**)
  module procedure :: power
end interface operator(**)

!> Size
interface size
  module procedure :: num_valid_digits
end interface size

contains

!> Assignment
pure subroutine assign(long, array)
  type(long_type(len=*)), intent(inout) :: long
  integer(int64), intent(in) :: array(:)
  integer(int64) :: n, i

  if (long%len < size(array)) error stop &
    & "[assign] Not enough memory."
  if (all(array >= 0_int64)) then
    long%sign = "+"
  else if (array(1) < 0_int64 .and. &
    & all(array(2:) >= 0_int64)) then
    long%sign = "-"
  else
    error stop "[assign] Invalid array."
  end if

  select case (size(array))
  case (0)
    long%start = long%len
    long%digit = 0
  case (1)
    long%start = long%len
    long%digit(:long%len - 1) = 0
    long%digit(long%len) = array(1)
  case (2:)
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
  end select
end subroutine assign

!> Number of digits
elemental function num_valid_digits(value1) result(ret)
  type(long_type(len=*)), intent(in) :: value1
  integer(int64) :: ret

  if (value1%len == 0) error stop "[num_valid_digits] Invalid number."
  ret = value1%len - value1%start + 1
end function num_valid_digits

!> Re-allocate long integer
pure subroutine re_allocate(long, len)
  type(long_type(len=:)), allocatable, intent(inout) :: long
  integer(int64), intent(in) :: len

  if (.not. allocated(long)) then
    allocate (long_type(len=len) :: long)
  else if (long%len /= len) then
    deallocate (long)
    allocate (long_type(len=len) :: long)
  end if
end subroutine re_allocate

!> Addition function
pure function add(value1, value2) result(ret)
  type(long_type(len=*)), intent(in) :: value1, value2
  type(long_type(len=:)), allocatable :: ret
  integer(int64) :: digit(value1%len)

  if (.not. (value1%len == value2%len)) error stop &
    & "[add] Invalid digit size."
  call re_allocate(ret, value1%len)
  if (value1%sign == value2%sign) then
    digit = value1%digit + value2%digit
    call carry(digit)
    call assign(ret, digit)
  else
    error stop "[add] Not supported yet."
  end if
end function add

pure function multiply(value1, value2) result(ret)
  type(long_type(len=*)), intent(in) :: value1, value2
  type(long_type(len=:)), allocatable :: ret
  integer(int64) :: i, digit(value1%len)

  if (.not. (value1%len == value2%len)) error stop &
    & "[multiply] Invalid digit size."
  call re_allocate(ret, value1%len)
  digit = 0
  do i = value2%start, size(value2%digit)
    digit = digit + cshift( &
      & value1%digit*value2%digit(i), size(value2%digit) - i)
  end do
  call carry(digit)
  call assign(ret, digit)
  ret%sign = merge("+", "-", value1%sign == value2%sign)
end function multiply

!> Exponential by squaring
pure recursive function exponential(y, x, n) result(ret)
  type(long_type(len=*)), intent(in) :: y, x
  integer(int64), intent(in) :: n
  type(long_type(len=:)), allocatable :: ret

  call re_allocate(ret, x%len)
  if (n == 0_int64) then
    ret = y
  else if (mod(n, 2_int64) == 0_int64) then
    ret = exponential(y, x*x, n/2)
  else if (mod(n, 2_int64) == 1_int64) then
    ret = exponential(x*y, x*x, (n - 1)/2)
  end if
end function exponential

pure function power(x, n) result(ret)
  type(long_type(len=*)), intent(in) :: x
  integer(int64), intent(in) :: n
  type(long_type(len=:)), allocatable :: ret, y

  call re_allocate(y, x%len)
  call re_allocate(ret, x%len)
  y = [1_int64]
  ret = exponential(y, x, n)
end function power

end module module_multiprecision

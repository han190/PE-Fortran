module module_utility

use, intrinsic :: iso_fortran_env, only: int64, real64
implicit none

public :: unit_digit
public :: num_digits
public :: sqrt
public :: lcm
public :: gcd
public :: carry
public :: to_array
public :: to_integer
public :: is_palindromic
public :: is_pandigital
public :: jagged_type
public :: swap
public :: re_allocate
private

!> Jagged type
type :: jagged_type
  integer(int64), allocatable :: array(:)
end type jagged_type

!> Square root of an integer (override sqrt)
interface sqrt
  module procedure :: sqrt_int64
end interface sqrt

!> Swap
interface swap
  module procedure :: swap_int64
  module procedure :: swap_char
end interface swap

!> Reallocation
interface re_allocate
  module procedure :: re_allocate_integer_1d
  module procedure :: re_allocate_logical_1d
end interface re_allocate

contains

!> Unit digit of a 64-bit integer
elemental integer(int64) function unit_digit(n)
  integer(int64), intent(in) :: n

  unit_digit = mod(n, 10_int64)
end function unit_digit

!> Number of digits of a 64-bit integer
elemental integer(int64) function num_digits(n)
  integer(int64), intent(in) :: n

  num_digits = floor(log10(real(n, real64))) + 1_int64
end function num_digits

!> Square root of a 64-bit integer
elemental integer(int64) function sqrt_int64(n)
  integer(int64), intent(in) :: n

  sqrt_int64 = floor(sqrt(real(n, real64)), int64)
end function sqrt_int64

!> Greatest common divisor of two int64 integers.
elemental recursive function gcd(a, b) result(ret)
  integer(int64), intent(in) :: a, b
  integer(int64) :: ret

  if (b /= 0_int64) then
    ret = gcd(b, mod(a, b))
  else
    ret = a
  end if
end function gcd

!> Least common multiple of two int64 integers.
elemental integer(int64) function lcm(a, b)
  integer(int64), intent(in) :: a, b

  lcm = abs(a*b)/gcd(a, b)
end function lcm

!> To tell if an int64 integer is palindromic.
elemental logical function is_palindromic(n, base)
  integer(int64), intent(in) :: n
  integer(int64), intent(in), optional :: base
  integer(int64) :: reversed, tmp, base_

  if (present(base)) then
    base_ = base
  else !> Default is base-10 number
    base_ = 10_int64
  end if

  reversed = 0_int64
  tmp = n
  do while (tmp > 0_int64)
    reversed = reversed*base_ + mod(tmp, base_)
    tmp = tmp/base_
  end do
  is_palindromic = n == reversed
end function is_palindromic

!> Carry
pure subroutine carry(digit, option)
  integer(int64), contiguous, intent(inout) :: digit(:)
  character, intent(in), optional :: option
  character :: option_
  integer(int64), allocatable :: tmp(:)

  !> "option" is optional, default is "+"
  if (present(option)) then
    option_ = option
  else
    option_ = "+"
  end if

  allocate (tmp(size(digit)))
  select case (option_)
  case ("+")
    do while (any(digit >= 10))
      tmp = merge(1, 0, digit >= 10)
      where (digit >= 10) digit = digit - 10
      digit = digit + cshift(tmp, 1)
    end do
  case ("-")
    do while (any(digit < 10))
      tmp = merge(1, 0, digit < 10)
      where (digit < 10) digit = digit + 10
      digit = digit - cshift(tmp, 1)
    end do
  case default
    error stop "[carry] Invalid option."
  end select
end subroutine carry

!> Convert number to array.
pure function to_array(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64), allocatable :: ret(:)
  integer(int64) :: i, len_, tmp

  tmp = n
  len_ = num_digits(tmp)
  call re_allocate(ret, len_)
  do i = len_, 1, -1
    ret(i) = unit_digit(tmp)
    tmp = tmp/10_int64
  end do
end function to_array

!> Convert array to integer
pure integer(int64) function to_integer(arr)
  integer(int64), intent(in) :: arr(:)
  integer(int64) :: i, tmp

  tmp = 0_int64
  do i = 1, size(arr)
    tmp = tmp*10_int64 + arr(i)
  end do
  to_integer = tmp
end function to_integer

!> Pandigital checker
pure logical function is_pandigital(n)
  integer(int64), intent(in) :: n
  integer(int64) :: tmp, l
  logical, allocatable :: arr(:)

  l = num_digits(n)
  if (l > 9_int64) l = 9_int64
  allocate (arr(l))

  arr = .false.
  tmp = n

  do
    associate (u => (unit_digit(tmp)))
      if (u == 0_int64 .or. u > l) exit
      arr(u) = .true.
    end associate
    tmp = tmp/10
  end do
  is_pandigital = count(arr) == l
end function is_pandigital

!> Swap integers
pure subroutine swap_int64(a, b)
  integer(int64), intent(inout) :: a, b
  integer(int64) :: tmp

  tmp = a
  a = b
  b = tmp
end subroutine swap_int64

!> Swap characters
pure subroutine swap_char(a, b)
  character(len=*), intent(inout) :: a, b
  character(len=len(a)) :: tmp

  tmp = a
  a = b
  b = tmp
end subroutine swap_char

!> Reallocate logical array
pure subroutine re_allocate_logical_1d(array, new_size)
  logical, allocatable, intent(inout) :: array(:)
  integer(int64), intent(in) :: new_size

  if (allocated(array)) then
    if (size(array) /= new_size) then
      deallocate (array)
      allocate (array(new_size))
    end if
  else
    allocate (array(new_size))
  end if
end subroutine re_allocate_logical_1d

!> Reallocate integer array
pure subroutine re_allocate_integer_1d(array, new_size)
  integer(int64), allocatable, intent(inout) :: array(:)
  integer(int64), intent(in) :: new_size

  if (allocated(array)) then
    if (size(array) /= new_size) then
      deallocate (array)
      allocate (array(new_size))
    end if
  else
    allocate (array(new_size))
  end if
end subroutine re_allocate_integer_1d

end module module_utility

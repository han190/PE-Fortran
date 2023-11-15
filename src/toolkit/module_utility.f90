module module_utility

use :: module_constant
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
private

!> Jagged type
type :: jagged_type
  integer(int32), allocatable :: array(:)
end type jagged_type

!> Unit digit of an integer
interface unit_digit
  module procedure :: unit_digit_int32
  module procedure :: unit_digit_int64
end interface unit_digit

!> Number of digits of an integer
interface num_digits
  module procedure :: num_digits_int32
  module procedure :: num_digits_int64
end interface num_digits

!> Square root of an integer (override sqrt)
interface sqrt
  module procedure :: sqrt_int32
  module procedure :: sqrt_int64
end interface sqrt

!> GCD
interface gcd
  module procedure :: gcd_int32
  module procedure :: gcd_int64
end interface gcd

!> LCM
interface lcm
  module procedure :: lcm_int32
  module procedure :: lcm_int64
end interface lcm

!> Palindromic checker
interface is_palindromic
  module procedure :: is_palindromic_int32
  module procedure :: is_palindromic_int64
end interface is_palindromic

!> Carry
interface carry
  module procedure :: carry_int32
  module procedure :: carry_int64
end interface carry

!> Convert integer to array
interface to_array
  module procedure :: to_array_int32
  module procedure :: to_array_int64
end interface to_array

!> Convert array to integer
interface to_integer
  module procedure :: to_integer_int32
  module procedure :: to_integer_int64
end interface to_integer

!> Is pandigital
interface is_pandigital
  module procedure :: is_palindromic_int32
  module procedure :: is_pandigital_int64
end interface is_pandigital

!> Swap
interface swap
  module procedure :: swap_int32
  module procedure :: swap_int64
  module procedure :: swap_char
end interface swap

contains

!> Unit digit of a 32-bit integer
elemental integer(int32) function unit_digit_int32(n)
  integer(int32), intent(in) :: n

  unit_digit_int32 = mod(n, 10_int32)
end function unit_digit_int32

!> Unit digit of a 64-bit integer
elemental integer(int64) function unit_digit_int64(n)
  integer(int64), intent(in) :: n

  unit_digit_int64 = mod(n, 10_int64)
end function unit_digit_int64

!> Number of digits of a 32-bit integer
elemental integer(int32) function num_digits_int32(n)
  integer(int32), intent(in) :: n

  num_digits_int32 = floor(log10(real(n, sp))) + 1_int32
end function num_digits_int32

!> Number of digits of a 64-bit integer
elemental integer(int64) function num_digits_int64(n)
  integer(int64), intent(in) :: n

  num_digits_int64 = floor(log10(real(n, sp))) + 1_int64
end function num_digits_int64

!> Square root of a 32-bit integer
elemental integer(int32) function sqrt_int32(n)
  integer(int32), intent(in) :: n

  sqrt_int32 = floor(sqrt(real(n, sp)), int32)
end function sqrt_int32

!> Square root of a 64-bit integer
elemental integer(int64) function sqrt_int64(n)
  integer(int64), intent(in) :: n

  sqrt_int64 = floor(sqrt(real(n, sp)), int64)
end function sqrt_int64

!> Greatest common divisor of two int32 integers.
elemental recursive function gcd_int32(a, b) result(ret)
  integer(int32), intent(in) :: a, b
  integer(int32) :: ret

  if (b == 0_int32) then
    ret = a
  else
    associate (r => mod(a, b))
      ret = gcd_int32(b, r)
    end associate
  end if
end function gcd_int32

!> Greatest common divisor of two int64 integers.
elemental recursive function gcd_int64(a, b) result(ret)
  integer(int64), intent(in) :: a, b
  integer(int64) :: ret

  if (b == 0_int64) then
    ret = a
  else
    associate (r => mod(a, b))
      ret = gcd_int64(b, r)
    end associate
  end if
end function gcd_int64

!> Least common multiple of two int32 integers.
elemental integer(int32) function lcm_int32(a, b)
  integer(int32), intent(in) :: a, b

  lcm_int32 = abs(a*b)/gcd_int32(a, b)
end function lcm_int32

!> Least common multiple of two int64 integers.
elemental integer(int64) function lcm_int64(a, b)
  integer(int64), intent(in) :: a, b

  lcm_int64 = abs(a*b)/gcd_int64(a, b)
end function lcm_int64

!> To tell if an int32 integer is palindromic.
elemental logical function is_palindromic_int32(n)
  integer(int32), intent(in) :: n
  integer(int32) :: reversed, tmp

  reversed = 0_int32
  tmp = n
  do while (tmp > 0_int32)
    reversed = reversed*10_int32 + mod(tmp, 10_int32)
    tmp = tmp/10_int32
  end do

  is_palindromic_int32 = .false.
  if (n == reversed) is_palindromic_int32 = .true.
end function is_palindromic_int32

!> To tell if an int64 integer is palindromic.
elemental logical function is_palindromic_int64(n)
  integer(int64), intent(in) :: n
  integer(int64) :: reversed, tmp

  reversed = 0_int64
  tmp = n
  do while (tmp > 0_int64)
    reversed = reversed*10_int64 + mod(tmp, 10_int64)
    tmp = tmp/10_int64
  end do

  is_palindromic_int64 = .false.
  if (n == reversed) is_palindromic_int64 = .true.
end function is_palindromic_int64

!> Carry
pure subroutine carry_int32(digs)
  integer(int32), contiguous, intent(inout) :: digs(:)
  integer(int32) :: tmp1(size(digs)), tmp2(size(digs))

  do while (any(digs >= 10))
    tmp1 = merge(1, 0, digs >= 10)
    tmp2 = merge(digs - 10, digs, digs >= 10)
    digs = tmp2 + cshift(tmp1, 1)
  end do
end subroutine carry_int32

!> Carry
pure subroutine carry_int64(digs)
  integer(int64), contiguous, intent(inout) :: digs(:)
  integer(int64) :: tmp1(size(digs)), tmp2(size(digs))

  do while (any(digs >= 10))
    tmp1 = merge(1, 0, digs >= 10)
    tmp2 = merge(digs - 10, digs, digs >= 10)
    digs = tmp2 + cshift(tmp1, 1)
  end do
end subroutine carry_int64

pure function to_array_int32(n) result(ret)
  integer(int32), intent(in) :: n
  integer(int32), allocatable :: ret(:)
  integer(int32) :: i, len_, tmp

  tmp = n
  len_ = num_digits_int32(tmp)
  allocate (ret(len_))
  do i = len_, 1, -1
    ret(i) = unit_digit(tmp)
    tmp = tmp/10_int32
  end do
end function to_array_int32

pure function to_array_int64(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64), allocatable :: ret(:)
  integer(int64) :: i, len_, tmp

  tmp = n
  len_ = num_digits_int64(tmp)
  allocate (ret(len_))
  do i = len_, 1, -1
    ret(i) = unit_digit(tmp)
    tmp = tmp/10_int64
  end do
end function to_array_int64

pure integer(int32) function to_integer_int32(arr)
  integer(int32), intent(in) :: arr(:)
  integer(int32) :: i, tmp

  tmp = 0_int32
  do i = 1, size(arr)
    tmp = tmp*10_int32 + arr(i)
  end do
  to_integer_int32 = tmp
end function to_integer_int32

pure integer(int64) function to_integer_int64(arr)
  integer(int64), intent(in) :: arr(:)
  integer(int64) :: i, tmp

  tmp = 0_int64
  do i = 1, size(arr)
    tmp = tmp*10_int64 + arr(i)
  end do
  to_integer_int64 = tmp
end function to_integer_int64

pure logical function is_pandigital_int32(n)
  integer(int32), intent(in) :: n
  integer(int32) :: tmp, l
  logical, allocatable :: arr(:)

  l = num_digits_int32(n)
  if (l > 9_int32) l = 9_int32
  allocate (arr(l))

  is_pandigital_int32 = .false.
  arr = .false.
  tmp = n

  do
    associate (u => (unit_digit(tmp)))
      if (u == 0_int32 .or. u > l) exit
      arr(u) = .true.
    end associate
    tmp = tmp/10
  end do

  if (count(arr) == l) is_pandigital_int32 = .true.
end function is_pandigital_int32

pure logical function is_pandigital_int64(n)
  integer(int64), intent(in) :: n
  integer(int64) :: tmp, l
  logical, allocatable :: arr(:)

  l = num_digits_int64(n)
  if (l > 9_int64) l = 9_int64
  allocate (arr(l))

  is_pandigital_int64 = .false.
  arr = .false.
  tmp = n

  do
    associate (u => (unit_digit(tmp)))
      if (u == 0_int64 .or. u > l) exit
      arr(u) = .true.
    end associate
    tmp = tmp/10
  end do

  if (count(arr) == l) is_pandigital_int64 = .true.
end function is_pandigital_int64

pure subroutine swap_int32(a, b)
  integer(int32), intent(inout) :: a, b
  integer(int32) :: tmp

  tmp = a
  a = b
  b = tmp
end subroutine swap_int32

pure subroutine swap_int64(a, b)
  integer(int64), intent(inout) :: a, b
  integer(int64) :: tmp

  tmp = a
  a = b
  b = tmp
end subroutine swap_int64

pure subroutine swap_char(a, b)
  character(len=*), intent(inout) :: a, b
  character(len=len(a)) :: tmp

  tmp = a
  a = b
  b = tmp
end subroutine swap_char

end module module_utility

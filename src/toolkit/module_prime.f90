module module_prime

use, intrinsic :: iso_fortran_env, only: int64, real64
use :: module_utility, only: sqrt, re_allocate
implicit none

public :: sift
public :: pack
public :: is_prime
public :: num_divisors
public :: prime_factorization
private

!> Pack
interface pack
  module procedure :: pack_primes
end interface pack

contains

!> Sift
pure function sift(n, option) result(check)
  integer(int64), intent(in) :: n
  character(len=*), intent(in), optional :: option
  logical, allocatable :: check(:)
  integer(int64) :: i
  character(len=:), allocatable :: option_

  if (present(option)) then
    option_ = trim(option)
  else
    option_ = "Eratosthenes"
  end if
  call re_allocate(check, n)  

  select case (trim(option_))
  case ("Eratosthenes")
    check(1) = .false.
    check(2) = .true.
    do i = 3, n
      check(i) = mod(i, 2_int64) /= 0
    end do
    do i = 2, sqrt(n)
      if (check(i)) check(i*i:n:i) = .false.
    end do
  case default
    error stop "[sift] Invalid sieve."
  end select
end function sift

!> Pack
function pack_primes(check, lower, upper) result(primes)
  logical, intent(in) :: check(:)
  integer(int64), intent(in), optional :: lower, upper
  integer(int64), allocatable :: primes(:)
  integer(int64) :: i, k, num_primes
  integer(int64) :: lower_, upper_

  if (present(lower)) then
    lower_ = lower
  else
    lower_ = 1
  end if

  if (present(upper)) then
    upper_ = upper
  else
    upper_ = size(check)
  end if

  num_primes = count(check(lower_:upper_))
  call re_allocate(primes, num_primes)

  k = 1
  do i = lower_, upper_
    if (check(i)) then
      primes(k) = i
      k = k + 1
    end if
  end do
end function pack_primes

!> 64-bit integer prime checker
elemental logical function is_prime(n)
  integer(int64), intent(in) :: n
  integer(int64) :: i

  select case (n)
  case (0:1)
    is_prime = .false.
    return
  case (2:3)
    is_prime = .true.
    return
  end select

  is_prime = .true.
  if (mod(n, 2_int64) == 0_int64) then
    is_prime = .false.
    return
  end if

  do i = 3_int64, sqrt(n) + 1_int64, 2_int64
    if (mod(n, i) == 0_int64) then
      is_prime = .false.
      return
    end if
  end do
end function is_prime

!> Prime factorization of a 64-bit integer
pure subroutine prime_factorization(n, primes, powers)
  integer(int64), intent(in) :: n
  integer(int64), intent(in) :: primes(:)
  integer(int64), intent(out) :: powers(size(primes))
  integer(int64) :: i, tmp
  character(len=500) :: error_message

  powers = 0
  tmp = n
  do i = 1, size(primes)
    do while (mod(tmp, primes(i)) == 0)
      powers(i) = powers(i) + 1_int64
      tmp = tmp/primes(i)
    end do
  end do
  
  if (tmp /= 1) then  
    write (error_message, "(a, 2(1x, i0))") &
      & "[prime_factorization] Not completely factorized.", n, tmp
    error stop trim(error_message)
  end if
end subroutine prime_factorization

!> Number of proper divisors of a 64-bit integer
pure integer(int64) function num_divisors(n, primes)
  integer(int64), intent(in) :: n
  integer(int64), intent(in) :: primes(:)
  integer(int64) :: powers(size(primes))

  call prime_factorization(n, primes, powers)
  num_divisors = product(powers + 1)
end function num_divisors

end module module_prime
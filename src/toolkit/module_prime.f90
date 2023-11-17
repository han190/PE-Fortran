module module_prime

use :: iso_fortran_env, only: int64, real64
use :: module_utility, only: sqrt
implicit none

public :: sieve_type
public :: sift
public :: pack
public :: is_prime
public :: num_divisors
public :: prime_factorization
private

!> Sieve type
type :: sieve_type(len)
  integer(int64), len :: len
  logical :: check(len)
end type sieve_type

!> Pack
interface pack
  module procedure :: pack_primes
end interface pack

contains

!> Sift
pure subroutine sift(sieve, check, option)
  type(sieve_type(len=*)), target, intent(inout) :: sieve
  logical, pointer, intent(inout), optional :: check(:)
  character(len=*), intent(in), optional :: option
  integer(int64) :: i
  character(len=:), allocatable :: option_

  if (sieve%len > huge(0_int64)) &
    & error stop "[sift] Invalid length."
  if (present(option)) then
    option_ = trim(option)
  else
    option_ = "Eratosthenes"
  end if
  
  select case (trim(option_))
  case ("Eratosthenes")
    sieve%check(1) = .false.
    sieve%check(2) = .true.
    do i = 3, sieve%len
      sieve%check(i) = mod(i, 2_int64) /= 0
    end do
    do i = 2, sqrt(sieve%len)
      if (sieve%check(i)) sieve%check(i*i:sieve%len:i) = .false.
    end do
  case default
    error stop "[sift] Invalid sieve."
  end select
  if (present(check)) check => sieve%check
end subroutine sift

!> Pack
function pack_primes(sieve, lower_bound, upper_bound) result(primes)
  type(sieve_type(len=*)), target, intent(inout) :: sieve
  integer(int64), intent(in), optional :: lower_bound, upper_bound
  integer(int64), allocatable :: primes(:)
  integer(int64) :: i, k, num_primes
  integer(int64) :: lower, upper

  if (present(lower_bound)) then
    lower = lower_bound
  else
    lower = 1
  end if

  if (present(upper_bound)) then
    upper = upper_bound
  else
    upper = size(sieve%check)
  end if

  num_primes = count(sieve%check(lower:upper))
  if (allocated(primes)) then
    if (size(primes) /= num_primes) then
      deallocate (primes)
      allocate (primes(num_primes))
    end if
  else
    allocate (primes(num_primes))
  end if

  k = 1
  do i = lower, upper
    if (sieve%check(i)) then
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

  powers = 0
  tmp = n
  do i = 1, size(primes)
    do while (mod(tmp, primes(i)) == 0)
      powers(i) = powers(i) + 1_int64
      tmp = tmp/primes(i)
    end do
  end do
  
  if (tmp /= 1) then
    block
      character(len=500) :: error_message
      write (error_message, "(a, 2(1x, i0))") &
        & "[prime_factorization] Not completely factorized.", n, tmp
      error stop trim(error_message)
    end block
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
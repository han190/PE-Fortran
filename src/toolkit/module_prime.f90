module module_prime

! use :: iso_fortran_env, only: int32, int64, real64
use :: module_constant
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
type :: sieve_type(len, kind)
  integer, len :: len
  integer, kind :: kind = int32
  logical :: check(len)
  integer(kind=kind), allocatable :: primes(:)
end type sieve_type

!> Apply a sieve
interface sift
  module procedure :: sift_int32
  module procedure :: sift_int64
end interface sift

!> Pack sieve type into prime array
interface pack
  module procedure :: pack_int32
  module procedure :: pack_int64
end interface pack

!> Prime checker
interface is_prime
  module procedure :: is_prime_int32
  module procedure :: is_prime_int64
end interface is_prime

!> Prime factorization
interface prime_factorization
  module procedure :: prime_factorization_int32
  module procedure :: prime_factorization_int64
end interface prime_factorization

!> Number of divisors
interface num_divisors
  module procedure :: num_divisors_int32
  module procedure :: num_divisors_int64
end interface num_divisors

contains

pure subroutine sift_int32(sieve, check, option)
  type(sieve_type(len=*, kind=int32)), target, intent(inout) :: sieve
  logical, pointer, intent(out), optional :: check(:)
  character(len=*), intent(in), optional :: option
  integer(int32) :: i
  character(len=:), allocatable :: option_

  if (sieve%len > huge(0_int32)) &
    & error stop "[sift_int32] Invalid length."
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
      sieve%check(i) = mod(i, 2_int32) /= 0
    end do
    do i = 2, sqrt(sieve%len)
      if (sieve%check(i)) sieve%check(i*i:sieve%len:i) = .false.
    end do
  case default
    error stop "[sift_int32] Invalid sieve."
  end select
  if (present(check)) allocate (check, source=sieve%check)
end subroutine sift_int32

pure subroutine sift_int64(sieve, check, option)
  type(sieve_type(len=*, kind=int64)), target, intent(inout) :: sieve
  logical, pointer, intent(out), optional :: check(:)
  character(len=*), intent(in), optional :: option
  integer(int64) :: i
  character(len=:), allocatable :: option_

  if (sieve%len > huge(0_int64)) &
    & error stop "[sift_int64] Invalid length."
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
    error stop "[sift_int64] Invalid sieve."
  end select
  if (present(check)) allocate (check, source=sieve%check)
end subroutine sift_int64

function pack_int32(sieve, lower_bound, upper_bound) result(primes)
  type(sieve_type(len=*, kind=int32)), target, intent(inout) :: sieve
  integer(int32), intent(in), optional :: lower_bound, upper_bound
  integer(int32), pointer :: primes(:)
  integer(int32) :: i, k, num_primes
  integer(int32) :: lower, upper
  character(len=:), allocatable :: option_

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
  if (allocated(sieve%primes)) then
    if (size(sieve%primes) /= num_primes) then
      deallocate (sieve%primes)
      allocate (sieve%primes(num_primes))
    end if
  else
    allocate (sieve%primes(num_primes))
  end if

  k = 1
  do i = lower, upper
    if (sieve%check(i)) then
      sieve%primes(k) = i
      k = k + 1
    end if
  end do
  allocate (primes, source=sieve%primes)
end function pack_int32

function pack_int64(sieve, lower_bound, upper_bound) result(primes)
  type(sieve_type(len=*, kind=int64)), target, intent(inout) :: sieve
  integer(int64), intent(in), optional :: lower_bound, upper_bound
  integer(int64), pointer :: primes(:)
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
  if (allocated(sieve%primes)) then
    if (size(sieve%primes) /= num_primes) then
      deallocate (sieve%primes)
      allocate (sieve%primes(num_primes))
    end if
  else
    allocate (sieve%primes(num_primes))
  end if

  k = 1
  do i = lower, upper
    if (sieve%check(i)) then
      sieve%primes(k) = i
      k = k + 1
    end if
  end do
  allocate (primes, source=sieve%primes)
end function pack_int64

!> 32-bit integer prime checker
elemental logical function is_prime_int32(n)
  integer(int32), intent(in) :: n
  integer(int32) :: i

  select case (n)
  case (0:1)
    is_prime_int32 = .false.
    return
  case (2:3)
    is_prime_int32 = .true.
    return
  end select

  is_prime_int32 = .true.
  if (mod(n, 2_int32) == 0_int32) then
    is_prime_int32 = .false.
    return
  end if

  do i = 3_int32, sqrt(n) + 1_int32, 2_int32
    if (mod(n, i) == 0_int32) then
      is_prime_int32 = .false.
      return
    end if
  end do
end function is_prime_int32

!> 64-bit integer prime checker
elemental logical function is_prime_int64(n)
  integer(int64), intent(in) :: n
  integer(int64) :: i

  select case (n)
  case (0:1)
    is_prime_int64 = .false.
    return
  case (2:3)
    is_prime_int64 = .true.
    return
  end select

  is_prime_int64 = .true.
  if (mod(n, 2_int64) == 0_int64) then
    is_prime_int64 = .false.
    return
  end if

  do i = 3_int64, sqrt(n) + 1_int64, 2_int64
    if (mod(n, i) == 0_int64) then
      is_prime_int64 = .false.
      return
    end if
  end do
end function is_prime_int64

!> Prime factorization of a 32-bit integer
pure subroutine prime_factorization_int32(n, primes, powers)
  integer(int32), intent(in) :: n
  integer(int32), intent(in) :: primes(:)
  integer(int32), intent(out) :: powers(size(primes))
  integer(int32) :: i, tmp

  powers = 0
  tmp = n
  do i = 1, size(primes)
    do while (mod(tmp, primes(i)) == 0)
      powers(i) = powers(i) + 1_int32
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
end subroutine prime_factorization_int32

!> Prime factorization of a 64-bit integer
pure subroutine prime_factorization_int64(n, primes, powers)
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
end subroutine prime_factorization_int64

!> Number of proper divisors of a 32-bit integer
pure integer(int32) function num_divisors_int32(n, primes)
  integer(int32), intent(in) :: n
  integer(int32), intent(in) :: primes(:)
  integer(int32) :: powers(size(primes))

  call prime_factorization_int32(n, primes, powers)
  num_divisors_int32 = product(powers + 1)
end function num_divisors_int32

!> Number of proper divisors of a 64-bit integer
pure integer(int64) function num_divisors_int64(n, primes)
  integer(int64), intent(in) :: n
  integer(int64), intent(in) :: primes(:)
  integer(int64) :: powers(size(primes))

  call prime_factorization_int64(n, primes, powers)
  num_divisors_int64 = product(powers + 1)
end function num_divisors_int64

end module module_prime
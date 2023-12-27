module module_test

use, intrinsic :: iso_fortran_env, only: int64
use :: euler_toolkit
implicit none

!> Test type
type :: test_type
  logical :: succeed = .true.
  character(:), allocatable :: name, message
  procedure(test_procedure), pointer :: test => null()
end type test_type

!> Test interface
abstract interface
  subroutine test_procedure(test)
    import :: test_type
    class(test_type), intent(inout) :: test
  end subroutine test_procedure
end interface

contains

!> Test suite
subroutine test_suites()
  character(len=*), parameter :: fmt = '("#", *(1x, a))'
  type(test_type), allocatable :: tests(:)
  integer :: stat, i

  allocate (tests(3))
  tests(1)%test => test_permutation
  tests(2)%test => test_prime
  tests(3)%test => test_multiprecision
  do i = 1, size(tests)
    call tests(i)%test()
    if (tests(i)%succeed) then
      print "('Test ', a, '... Passed.')", tests(i)%name
    else
      error stop tests(i)%message
    end if
  end do
end subroutine test_suites

!> Test permutation
subroutine test_permutation(test)
  class(test_type), intent(inout) :: test
  type(permutation_type) :: permutation
  integer(int64), allocatable :: indices(:), cycles(:)
  integer(int64) :: n, k

  n = 10_int64
  k = 3_int64
  permutation = new_permutation(n, k)

  test%name = "permutation"
  test%succeed = .true.
  test%message = "[test_permutation] Success."
  do while (permutable(permutation))
    indices = index(permutation)
    cycles = permutation%cycles
    call set_permutation(permutation, indices)
    if (.not. all(permutation%cycles == cycles)) then
      test%succeed = .false.
      test%message = "[test_permutation] Failed."
      exit
    end if
  end do
end subroutine test_permutation

!> Test primes
subroutine test_prime(test)
  class(test_type), intent(inout) :: test
  logical, allocatable :: check(:)
  integer(int64), allocatable :: primes(:)
  integer(int64) :: i

  check = sift(1000000_int64, "Eratosthenes")
  primes = pack(check)

  test%name = "prime"
  test%succeed = .true.
  test%message = "[test_prime] Success."
  do i = 1, size(primes)
    if (.not. is_prime(primes(i))) then
      test%succeed = .false.
      test%message = "[test_prime] Failed."
      exit
    end if
  end do
end subroutine test_prime

!> Test multiprecision
subroutine test_multiprecision(test)
  class(test_type), intent(inout) :: test
  type(long_type) :: x, y

  test%name = "multiprecision"
  test%message = ""
  call initialize(x, 11_int64)
  call initialize(y, 11_int64)
  x = [integer(int64) :: 4, 6, 3, 5, 1, 0, 7, 9, 8, 2]
  y = [integer(int64) :: 3, 2, 4, 1, 5, 7, 9, 8, 6, 0]
  x = x + y
  test%succeed = 7876687842_int64 == to_integer(x%digit)
end subroutine test_multiprecision

end module module_test
submodule(module_problem) submodule_euler0037
implicit none
contains

module subroutine euler0037(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 1000000_int64
  type(sieve_type(len=:)), allocatable :: sieve
  integer(int64) :: i, knt, sln
  logical, pointer :: check(:) => null()

  allocate (sieve_type(len=n) :: sieve)
  call sift(sieve, check=check)
  i = 10
  knt = 0
  sln = 0

  do
    if (knt == 11 .or. i == n) exit
    if (check(i) .and. is_trunc(i, check)) then
      sln = sln + i
      knt = knt + 1
    end if
    i = i + 1
  end do
  write (problem%answer, "(i20)") sln
  nullify (check)
end subroutine euler0037

pure logical function is_trunc(n, primes)
  integer(int64), intent(in) :: n
  logical, intent(in) :: primes(:)

  is_trunc = is_left_trunc(n, primes) .and. is_right_trunc(n, primes)
end function is_trunc

pure logical function is_left_trunc(n, primes)
  integer(int64), intent(in) :: n
  logical, intent(in) :: primes(:)
  integer(int64) :: tmp

  tmp = 10_int64
  do while (tmp < n .and. mod(n, tmp) >= 1)
    if (.not. primes(mod(n, tmp))) then
      is_left_trunc = .false.
      return
    end if
    tmp = tmp*10_int64
  end do
  is_left_trunc = .true.
end function is_left_trunc

pure logical function is_right_trunc(n, primes)
  integer(int64), intent(in) :: n
  logical, intent(in) :: primes(:)
  integer(int64) :: tmp

  tmp = n
  do while (tmp > 0_int64)
    if (primes(tmp)) then
      tmp = tmp/10_int64
    else
      is_right_trunc = .false.
      return
    end if
  end do
  is_right_trunc = .true.
end function is_right_trunc

end submodule submodule_euler0037

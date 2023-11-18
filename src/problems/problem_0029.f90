submodule(module_problem) submodule_euler0029
implicit none
integer(int64), parameter :: error = -99999
contains

module subroutine euler0029(problem)
  class(problem_type), intent(inout) :: problem

  write (problem%answer, "(i20)") solve()
end subroutine euler0029

integer(int64) function solve()
  integer(int64), parameter :: upper = 100
  integer(int64), parameter :: limit = upper*6 ! 2**7 > 100
  integer(int64) :: i, j, tmp(2)
  logical, allocatable :: check(:)
  integer(int64), allocatable :: primes(:)
  logical, allocatable :: array(:, :)

  check = sift(1000_int64)
  primes = pack(check)
  allocate (array(limit, limit))
  array = .false.

  do i = 2, 100
    do j = 2, 100
      tmp = convert_base_power(i, j, primes)
      array(tmp(1), tmp(2)) = .true.
    end do
  end do
  solve = count(array)
end function solve

pure function convert_base_power(i, j, primes) result(ret)
  integer(int64), intent(in) :: i, j, primes(:)
  integer(int64) :: ret(2), tmp(2)

  call is_power(i, primes, tmp)
  if (all(tmp == error)) then
    ret(1) = i
    ret(2) = j
  else
    ret(1) = tmp(1)
    ret(2) = tmp(2)*j
  end if
end function convert_base_power

pure subroutine is_power(n, primes, base_power)
  integer(int64), intent(in) :: n, primes(:)
  integer(int64), intent(out) :: base_power(2)
  integer(int64) :: powers(size(primes))

  base_power = error
  call prime_factorization(n, primes, powers)
  associate (p => pack(primes, powers /= 0), &
    & e => pack(powers, powers /= 0))
    if (all(e == e(1))) base_power = [product(p), e(1)]
  end associate
end subroutine is_power

end submodule submodule_euler0029

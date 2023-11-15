submodule(module_interface) submodule_euler0029
implicit none
integer(int32), parameter :: error = -99999
contains

module subroutine euler0029(problem)
  type(problem_type), intent(inout) :: problem

  write (problem%answer, "(i20)") solve()
end subroutine euler0029

integer(int32) function solve()
  integer(int32), parameter :: upper = 100
  integer(int32), parameter :: limit = upper*6 ! 2**7 > 100
  integer(int32) :: i, j, tmp(2)
  type(sieve_type(len=:)), allocatable :: sieve
  integer(int32), pointer :: primes(:) => null()
  logical, allocatable :: array(:, :)

  allocate (sieve_type(len=1000) :: sieve)
  call sift(sieve)
  primes => pack(sieve)
  allocate (array(limit, limit))
  array = .false.

  do i = 2, 100
    do j = 2, 100
      tmp = convert_base_power(i, j, primes)
      array(tmp(1), tmp(2)) = .true.
    end do
  end do
  solve = count(array)
  nullify (primes)
end function solve

pure function convert_base_power(i, j, primes) result(ret)
  integer(int32), intent(in) :: i, j, primes(:)
  integer(int32) :: ret(2), tmp(2)

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
  integer(int32), intent(in) :: n, primes(:)
  integer(int32), intent(out) :: base_power(2)
  integer(int32) :: powers(size(primes))

  base_power = error
  call prime_factorization(n, primes, powers)
  associate (p => pack(primes, powers /= 0), &
    & e => pack(powers, powers /= 0))
    if (all(e == e(1))) base_power = [product(p), e(1)]
  end associate
end subroutine is_power

end submodule submodule_euler0029

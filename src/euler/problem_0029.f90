submodule(interface_module) euler_problem_0029_submodule
implicit none
integer(i32), parameter :: error = -99999

contains

module character(len=20) function euler0029()
  write (euler0029, "(i20)") answer()
end function euler0029

pure integer(i32) function answer()
  use prime_module, only: Sieve_of_Eratosthenes
  implicit none

  integer(i32), parameter :: upper = 100, lower = 2
  integer(i32), parameter :: limit = upper*6 ! 2**7 > 100
  integer(i32) :: i, j, tmp(2)
  integer(i32), allocatable :: primes(:)
  logical, allocatable :: array(:, :)

  allocate (array(limit, limit))
  call Sieve_of_Eratosthenes(upper, primes)
  array = .false.

  do i = lower, upper
    do j = lower, upper
      associate (value_ => ([i, j])) ! To avoid tmp array.
        tmp = convert_base_power(value_, primes)
      end associate
      array(tmp(1), tmp(2)) = .true.
    end do
  end do
  answer = count(array)
end function answer

pure function convert_base_power(val, primes) result(ret)
  integer(i32), intent(in) :: val(2), primes(:)
  integer(i32) :: ret(2), tmp(2)

  call is_power(val(1), primes, tmp)
  ret = merge(val, [tmp(1), tmp(2)*val(2)], all(tmp == error))
end function convert_base_power

pure subroutine is_power(n, primes, base_power)
  use prime_module, only: prime_factorization
  implicit none

  integer(i32), intent(in) :: n, primes(:)
  integer(i32), intent(out) :: base_power(2)
  integer(i32) :: powers(size(primes))

  base_power = error
  call prime_factorization(n, primes, powers)
  associate (p => pack(primes, powers /= 0), &
             e => pack(powers, powers /= 0))
    if (all(e == e(1))) base_power = [product(p), e(1)]
  end associate
end subroutine is_power

end submodule euler_problem_0029_submodule

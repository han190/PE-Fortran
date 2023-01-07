submodule(interface_module) euler_problem_0046_submodule
implicit none

contains

module character(len=20) function euler0046()
  write (euler0046, "(i20)") answer()
end function euler0046

pure integer(i32) function answer()
  use prime_module, only: sieve_of_Eratosthenes
  implicit none

  integer(i32), parameter :: n = 10000
  integer(i32) :: j, res
  logical :: not_found
  logical, allocatable :: is_prime(:)

  allocate (is_prime(n))
  call sieve_of_Eratosthenes(n, is_prime)
  res = 1; not_found = .true.

  outer: do while (not_found)
    res = res + 2
    j = 2
    not_found = .false.

    inner: do while (res >= j)
      if (is_prime(j) .and. is_twice_square(res - j)) then
        not_found = .true.
        exit inner
      end if
      j = j + 1
    end do inner
  end do outer

  answer = res
end function answer

elemental logical function is_twice_square(n)
  integer, intent(in) :: n
  real(dp) :: sqrt_nover2

  is_twice_square = .false.
  sqrt_nover2 = sqrt(0.5_dp*real(n, dp))

  if (sqrt_nover2 - real(floor(sqrt_nover2), dp) < tiny_dp) &
    is_twice_square = .true.
end function is_twice_square

end submodule euler_problem_0046_submodule

submodule(interface_module) euler_problem_0050_submodule
implicit none

contains

module character(len=20) function euler0050()
  write (euler0050, "(i20)") answer()
end function euler0050

pure integer(i64) function answer()
  use prime_module, only: Sieve_of_Eratosthenes
  implicit none

  integer(i64), parameter :: upper_ = 1000000
  integer(i64), allocatable :: primes(:)
  integer(i64) :: n, i, j

  call Sieve_of_Eratosthenes(upper_, primes)
  n = 0
  do while (sum(primes(1:n)) <= upper_)
    n = n + 1
  end do

  outer: do i = n - 1, 1, -1
    answer = sum(primes(1:i)); j = 1
    inner: do
      if (any(answer == primes)) exit outer
      answer = answer - primes(j)
      j = j + 1
    end do inner
  end do outer
end function answer

end submodule euler_problem_0050_submodule

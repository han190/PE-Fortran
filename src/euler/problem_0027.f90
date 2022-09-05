submodule(interface_module) euler_problem_0027_submodule
  implicit none

contains

  module character(len=20) function euler0027()
    write (euler0027, "(i20)") answer()
  end function euler0027

  pure integer(i32) function answer()
    answer = -(2*30 + 1)*quadratic_primes(1, 41, 30)
  end function answer

  pure integer(i32) function quadratic_primes(i, j, n)
    integer(i32), intent(in) :: i, j, n
    integer(i32), parameter :: k = 1000

    quadratic_primes = merge(0, n*(n + i) + j, abs(i) >= k .or. abs(j) > k)
  end function quadratic_primes

end submodule euler_problem_0027_submodule

submodule(interface_module) euler_problem_0058_submodule
  implicit none

contains

  module character(len=20) function euler0058()
    write (euler0058, "(i20)") answer()
  end function euler0058

  integer(i64) function answer()
    use prime_module, only: is_prime
    implicit none

    integer(i64) :: i, j, n

    j = 0; n = 2
    do
      associate (array => diagonal(n))
        do i = 1, 3
          if (is_prime(array(i))) j = j + 1
        end do
      end associate

      associate (primes => real(j), total => real((n - 1)*4 + 1))
        if (primes/total < 0.1_sp) exit
      end associate
      n = n + 1
    end do
    answer = side_length(n)
  end function answer

  pure function diagonal(n) result(ret)
    integer(i64), intent(in) :: n
    integer(i64), allocatable :: ret(:)
    integer(i64) :: i

    ret = (n*2 - 1)**2 - [(i*2*(n - 1), i=3, 0, -1)]
  end function diagonal

  elemental integer(i64) function side_length(n)
    integer(i64), intent(in) :: n

    side_length = n*2 - 1
  end function side_length

end submodule euler_problem_0058_submodule

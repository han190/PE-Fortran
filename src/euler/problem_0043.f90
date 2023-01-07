submodule(interface_module) euler_problem_0043_submodule
implicit none

contains

module character(len=20) function euler0043()
  write (euler0043, "(i20)") answer()
end function euler0043

integer(i64) function answer()
  integer(i64) :: i, array(10)

  array = [[0], [(i, i=9, 1, -1)]]
  answer = 0

  do while (next_permute(array))
    associate (tmp => to_integer(array))
      if (divisible(tmp)) answer = answer + tmp
    end associate
  end do
end function answer

pure logical function divisible(n)
  integer(i64), intent(in) :: n
  integer(i64) :: primes(7), tmp, i, x

  primes = [17, 13, 11, 7, 5, 3, 2]
  tmp = n

  do i = 1, 7
    x = mod(tmp, 1000_i64)
    divisible = mod(x, primes(i)) == 0
    if (.not. divisible) exit
    tmp = tmp/10_i64
  end do
end function divisible

end submodule euler_problem_0043_submodule

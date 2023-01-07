submodule(interface_module) euler_problem_0057_submodule

contains

module character(len=20) function euler0057()
  write (euler0057, "(i20)") answer()
end function euler0057

pure integer(i32) function answer()
  use multiprecision_module
  implicit none

  integer(i32), parameter :: max_ = 1000
  integer(i32) :: i
  type(long_integer) :: n, d ! numerator, denominator

  answer = 0; n = 1; d = 2
  do i = 1, max_
    n = n + d*2
    call swap(n, d)
    if (len(n + d) > len(d)) answer = answer + 1
  end do
end function answer

end submodule euler_problem_0057_submodule

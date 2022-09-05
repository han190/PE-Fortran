submodule(interface_module) euler_problem_0016_submodule
  implicit none

contains

  module character(len=20) function euler0016()
    write (euler0016, "(i20)") answer()
  end function euler0016

  !> There are faster ways to do this, but with an already
  !> implemented multiprecision integer library, this is
  !> the most cost-efficient way.
  pure integer(i32) function answer()
    use multiprecision_module
    implicit none
    type(long_integer) :: x

    x = int(2, "ll")**1000
    answer = sum(x%digit)
  end function answer

end submodule euler_problem_0016_submodule

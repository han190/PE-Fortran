submodule(interface_module) euler_problem_0006_submodule
  implicit none

contains

  module character(len=20) function euler0006()
    write (euler0006, "(i20)") answer()
  end function euler0006

  elemental integer(i32) function answer()
    integer(i32), parameter :: n = 100
    integer(i32) :: i

    associate (array => [(i, i=1, n)])
      answer = sum(array)**2 - sum(array**2)
    end associate
  end function answer

end submodule euler_problem_0006_submodule

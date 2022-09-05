submodule(interface_module) euler_problem_0030_submodule
  implicit none

contains

  module character(len=20) function euler0030()
    write (euler0030, "(i20)") answer()
  end function euler0030

  elemental integer(i32) function answer()
    integer(i32), parameter :: n = 999999
    integer(i32) :: i

    answer = 0
    do i = 2, n
      if (sum(to_array(i)**5) == i) answer = answer + i
    end do
  end function answer

end submodule euler_problem_0030_submodule

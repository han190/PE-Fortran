submodule(interface_module) euler_problem_0004_submodule
  implicit none

contains

  module character(len=20) function euler0004()
    write (euler0004, "(i20)") answer()
  end function euler0004

  elemental integer(i32) function answer()
    integer(i32), parameter :: start = 100, end = 999
    integer(i32) :: i, j

    answer = 0
    do i = start, end
      do j = start, end
        if (is_palindromic(i*j) .and. i*j > answer) answer = i*j
      end do
    end do
  end function answer

end submodule euler_problem_0004_submodule

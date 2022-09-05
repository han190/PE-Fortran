submodule(interface_module) euler_problem_0056_submodule
  use multiprecision_module
  implicit none

contains

  module character(len=20) function euler0056()
    write (euler0056, "(i20)") answer()
  end function euler0056

  elemental integer(i32) function answer()
    integer(i32), parameter :: const = 89, max_ = 10
    type(long_integer) :: integers(max_)
    integer(i32) :: i, j, sum_(max_, max_)

    integers = [(int(const + i, "ll")**const, i=1, max_)]
    do j = 1, max_
      do i = 1, max_
        integers(i) = int(const + i, "ll")*integers(i)
        sum_(i, j) = sum(integers(i)%digit)
      end do
    end do
    answer = maxval(sum_)
  end function answer

end submodule euler_problem_0056_submodule

submodule(module_interface) submodule_euler0039
implicit none
contains

module subroutine euler0039(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 1000
  integer(int64) :: i

  write (problem%answer, "(i20)") maxloc([(right_triangle(i), i=1, n)], dim=1)
end subroutine euler0039

elemental integer(int64) function right_triangle(p)
  integer(int64), intent(in) :: p
  integer(int64) :: i, j, k

  right_triangle = 0
  do i = 1, p/2 + 1
    do j = 1, p - i - 1
      k = p - i - j
      if (i**2 + j**2 == k**2) right_triangle = right_triangle + 1
    end do
  end do
end function right_triangle

end submodule submodule_euler0039

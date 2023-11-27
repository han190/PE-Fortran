submodule(module_problem) submodule_euler0045
implicit none
contains

module subroutine euler0045(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: x, y

  call hexagonal_pentagonal(3_int64, x, y)
  write (problem%answer, "(i20)") y*(2*y - 1)
end subroutine euler0045

pure recursive subroutine hexagonal_pentagonal(i, x, y)
  integer(int64), intent(in) :: i
  integer(int64), intent(out) :: x, y
  integer(int64) :: a, b

  if (i == 1) then
    x = 1
    y = 1
  else
    call hexagonal_pentagonal(i - 1, a, b)
    x = 97*a + 112*b - 44
    y = 84*a + 97*b - 38
  end if
end subroutine hexagonal_pentagonal

end submodule submodule_euler0045
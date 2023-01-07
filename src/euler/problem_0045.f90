submodule(interface_module) euler_problem_0045_submodule
implicit none

contains

module character(len=20) function euler0045()
  write (euler0045, "(i20)") answer()
end function euler0045

pure integer(i64) function answer()
  integer(i64) :: x, y

  call hexagonal_pentagonal(3_i64, x, y)
  answer = y*(2*y - 1)
end function answer

pure recursive subroutine hexagonal_pentagonal(i, x, y)
  integer(i64), intent(in) :: i
  integer(i64), intent(out) :: x, y
  integer(i64) :: a, b

  if (i == 1) then
    x = 1
    y = 1
  else
    call hexagonal_pentagonal(i - 1, a, b)
    x = 97*a + 112*b - 44
    y = 84*a + 97*b - 38
  end if
end subroutine hexagonal_pentagonal

end submodule euler_problem_0045_submodule

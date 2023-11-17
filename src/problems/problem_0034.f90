submodule(module_interface) submodule_euler0034
implicit none
contains

module subroutine euler0034(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: i, sln

  sln = 0
  do i = 1, 40585
    if (is_curious(i)) sln = sln + i
  end do
  write (problem%answer, "(i20)") sln - 3
end subroutine euler0034

elemental logical function is_curious(n)
  integer(int64), intent(in) :: n
  integer(int64) :: i, tmp
  integer(int64), allocatable :: arr(:)

  arr = to_array(n)
  tmp = 0
  do i = 1, size(arr)
    tmp = tmp + factorial(arr(i))
  end do
  is_curious = tmp == n
end function is_curious

elemental integer(int64) function factorial(n)
  integer(int64), intent(in) :: n

  factorial = int(gamma(real(n + 1)), int64)
end function factorial

end submodule submodule_euler0034
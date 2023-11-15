submodule(module_interface) submodule_euler0034
implicit none
contains

module subroutine euler0034(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32) :: i, sln

  sln = 0
  do i = 1, 40585
    if (is_curious(i)) sln = sln + i
  end do
  write (problem%answer, "(i20)") sln - 3
end subroutine euler0034

elemental logical function is_curious(n)
  integer(int32), intent(in) :: n
  integer(int32) :: i, tmp
  integer(int32), allocatable :: arr(:)

  arr = to_array(n)
  tmp = 0
  do i = 1, size(arr)
    tmp = tmp + factorial(arr(i))
  end do
  is_curious = tmp == n
end function is_curious

elemental integer(int32) function factorial(n)
  integer(int32), intent(in) :: n

  factorial = int(gamma(real(n + 1)), int32)
end function factorial

end submodule submodule_euler0034
submodule(module_interface) submodule_euler0015
implicit none
contains

module subroutine euler0015(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32), parameter :: n = 20
  integer(int32) :: i
  integer(int64) :: sln

  sln = 1
  do i = 1, n
    sln = sln*(n + i)/i
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0015

end submodule submodule_euler0015

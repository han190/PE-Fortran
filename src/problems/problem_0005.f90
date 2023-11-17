submodule(module_problem) submodule_euler0005
implicit none
contains

module subroutine euler0005(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 20
  integer(int64) :: i, sln

  sln = 1
  do i = 1, n
    sln = lcm(sln, i)
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0005

end submodule submodule_euler0005

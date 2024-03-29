submodule(module_problem) submodule_euler0006
implicit none
contains

module subroutine euler0006(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 100
  integer(int64) :: i, sln

  associate (array => [(i, i=1, n)])
    sln = sum(array)**2 - sum(array**2)
  end associate
  write (problem%answer, "(i20)") sln
end subroutine euler0006

end submodule submodule_euler0006

submodule(module_problem) submodule_euler0010
implicit none
contains

module subroutine euler0010(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 2000000
  logical, allocatable :: check(:)
  integer(int64) :: i, sln

  check = sift(n, "Eratosthenes")
  sln = 0
  do i = 1, n
    if (check(i)) sln = sln + i
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0010

end submodule submodule_euler0010

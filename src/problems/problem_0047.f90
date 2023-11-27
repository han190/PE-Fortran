submodule(module_problem) submodule_euler0047
implicit none
contains

module subroutine euler0047(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 1000000 - 1
  integer(int64) :: i, goal, sln
  integer(int64), allocatable :: n_factor(:)

  allocate (n_factor(n + 1))
  n_factor = 0

  do i = 2, n
    if (n_factor(i) == 0) n_factor(2*i:n:i) = n_factor(2*i:n:i) + 1
  end do

  goal = 4
  do sln = 2, n
    if (all(n_factor(sln:sln + 3) == goal)) exit
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0047

end submodule submodule_euler0047

submodule(module_problem) submodule_euler0041
implicit none
contains

module subroutine euler0041(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 7654321
  logical, allocatable :: check(:)
  integer(int64) :: i, sln

  check = sift(n)
  do i = n, 1, -1
    if (check(i) .and. is_pandigital(i)) then
      sln = i
      exit
    end if
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0041

end submodule submodule_euler0041

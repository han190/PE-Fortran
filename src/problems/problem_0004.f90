submodule(module_interface) submodule_euler0004
implicit none
contains

module subroutine euler0004(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: start = 100, end = 999
  integer(int64) :: i, j, sln

  sln = 0
  do i = start, end
    do j = start, end
      if (is_palindromic(i*j) .and. i*j > sln) sln = i*j
    end do
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0004

end submodule submodule_euler0004

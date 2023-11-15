submodule(module_interface) submodule_euler0028
implicit none
contains

module subroutine euler0028(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32) :: i, j, sln

  sln = 1
  do i = 2, 501
    do j = 0, 3
      sln = sln + 2*(1 - i)*j + (2*i - 1)**2
    end do
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0028

end submodule submodule_euler0028
submodule(module_problem) submodule_euler0009
implicit none
contains

module subroutine euler0009(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 1000
  integer(int64) :: i, j, sln

  sln = 0
  do i = 1, n
    do j = i + 1, n
      associate (k => (n - i - j))
        if (.not. j < k) cycle
        if (i**2 + j**2 == k**2) then
          sln = i*j*k
          exit
        end if
      end associate
    end do
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0009

end submodule submodule_euler0009

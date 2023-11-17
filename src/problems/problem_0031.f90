submodule(module_interface) submodule_euler0031
implicit none
contains

module subroutine euler0031(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 200
  integer(int64) :: coins(0:7), array(0:7, 0:n), i, j, sln

  coins = [1, 2, 5, 10, 20, 50, 100, 200]
  array(1:7, :) = 0
  array(0, :) = 1

  do j = 0, n
    do i = 1, 7
      array(i, j) = array(i - 1, j)
      if (j >= coins(i)) array(i, j) = array(i, j) + &
        & array(i, j - coins(i))
    end do
  end do
  sln = array(7, n)
  write (problem%answer, "(i20)") sln
end subroutine euler0031

end submodule submodule_euler0031

submodule(module_problem) submodule_euler0007
implicit none
contains

module subroutine euler0007(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 10000
  integer(int64) :: i, sln

  i = 0
  sln = 0
  do while (i <= n)
    sln = sln + 1
    if (mod(sln, 2) == 2 .and. sln /= 2) then
      cycle
    else if (is_prime(sln)) then
      i = i + 1
    end if
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0007

end submodule submodule_euler0007

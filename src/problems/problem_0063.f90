submodule(module_problem) submodule_euler0063
implicit none
contains

module subroutine euler0063(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: i, k, sln

  sln = 0
  do i = 1, 9
    associate (r => real(i, real64), log10 => log(10.))
      k = floor(log10/(log10 - log(r)))
      sln = sln + k
    end associate
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0063

end submodule submodule_euler0063

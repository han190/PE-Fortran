submodule(module_problem) submodule_euler0057
implicit none
contains

module subroutine euler0057(problem)
  type(problem_type), intent(inout) :: problem
  real(real64), parameter :: delta = log10(1._real64 + sqrt(2._real64))
  real(real64) :: logp, logq
  integer(int64) :: sln, i

  logp = delta - log10(2._real64)
  logq = delta - log10(2._real64)*3.0/2.0
  sln = 0
  do i = 1, 1000
    logp = logp + delta
    logq = logq + delta
    if (floor(logp) > floor(logq)) sln = sln + 1
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0057

end submodule submodule_euler0057

submodule(module_problem) submodule_euler0097
implicit none
contains

module subroutine euler0097(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: i, p, sln

  sln = 2
  p = 10_int64**10
  do i = 1, 7830457 - 1
    sln = merge(sln*2, mod(sln*2, p), sln < p)
  end do
  sln = mod(sln*28433 + 1, p)
  write (problem%answer, "(i0)") sln
end subroutine euler0097

end submodule submodule_euler0097

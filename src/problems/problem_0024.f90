submodule(module_problem) submodule_euler0024
implicit none
contains

module subroutine euler0024(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), allocatable :: tmp(:)
  integer(int64) :: num_permutes(10)
  integer(int64) :: n, i, j, sln

  !> Number of permutations for n digit is n!
  num_permutes = [(int(gamma(real(i + 1)), int64), i=0, 9)]
  tmp = [(i, i=0, 9)]
  n = 10**num_digits(num_permutes(10)) - 1

  sln = 0
  do i = 10, 1, -1
    j = tmp(n/num_permutes(i) + 1)
    n = mod(n, num_permutes(i))
    tmp = pack(tmp, tmp /= j)
    sln = sln + j*10**(i - 1)
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0024

end submodule submodule_euler0024

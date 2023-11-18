submodule(module_problem) submodule_euler0012
implicit none
contains

module subroutine euler0012(problem)
  class(problem_type), intent(inout) :: problem
  logical, allocatable :: check(:)
  integer(int64), allocatable :: primes(:)
  integer(int64) :: i, sln

  check = sift(50000_int64)
  primes = pack(check)
  i = 0
  sln = 0

  do
    i = i + 1
    sln = sln + i
    if (num_divisors(sln, primes) > 500) exit
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0012

end submodule submodule_euler0012

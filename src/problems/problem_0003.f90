submodule(module_problem) submodule_euler0003
implicit none
contains

module subroutine euler0003(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 600851475143_real64
  logical, allocatable :: check(:)
  integer(int64), allocatable :: primes(:)
  integer(int64) :: i, num_checks

  check = sift(sqrt(n), "Eratosthenes")
  primes = pack(check)
  do i = size(primes), 1, -1
    if (mod(n, primes(i)) == 0) exit
  end do
  write (problem%answer, "(i20)") primes(i)
end subroutine euler0003

end submodule submodule_euler0003

submodule(module_interface) submodule_euler0012
implicit none
contains

module subroutine euler0012(problem)
  type(problem_type), intent(inout) :: problem
  type(sieve_type(len=:)), allocatable :: sieve
  integer(int32), pointer :: primes(:) => null()
  integer(int32) :: i, sln

  allocate (sieve_type(len=50000) :: sieve)
  call sift(sieve)
  primes => pack(sieve)
  i = 0
  sln = 0

  do
    i = i + 1
    sln = sln + i
    if (num_divisors(sln, primes) > 500) exit
  end do
  write (problem%answer, "(i20)") sln
  nullify (primes)
end subroutine euler0012

end submodule submodule_euler0012

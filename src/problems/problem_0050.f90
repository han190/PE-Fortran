submodule(module_interface) submodule_euler0050
implicit none
contains

module subroutine euler0050(problem)
  type(problem_type), intent(inout) :: problem
  type(sieve_type(len=:)), allocatable :: sieve
  integer(int64), parameter :: num_sieves = 1000000
  logical, pointer :: check(:) => null()
  integer(int64), allocatable :: primes(:)
  integer(int64) :: n, i, j, sln

  allocate (sieve_type(len=num_sieves) :: sieve)
  call sift(sieve, check=check)
  primes = pack(sieve)
  
  n = 0
  do while (sum(primes(1:n)) <= num_sieves)
    n = n + 1
  end do

  outer: do i = n - 1, 1, -1
    sln = sum(primes(1:i))
    j = 1
    inner: do
      if (any(sln == primes)) exit outer
      sln = sln - primes(j)
      j = j + 1
    end do inner
  end do outer
  write (problem%answer, "(i20)") sln
  nullify (check)
end subroutine euler0050

end submodule submodule_euler0050

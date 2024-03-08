submodule(module_problem) submodule_euler0050
implicit none
contains

module subroutine euler0050(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: limit = 1000000
  logical, allocatable :: check(:)
  integer(int64), allocatable :: primes(:)
  integer(int64) :: n, i, j, sln

  check = sift(limit)
  primes = pack(check)
  
  n = 0
  do while (sum(primes(1:n)) <= limit)
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
  write (answer, "(i20)") sln
end subroutine euler0050

end submodule submodule_euler0050

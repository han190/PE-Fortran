submodule(module_interface) submodule_euler0010
implicit none
contains

module subroutine euler0010(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 2000000
  type(sieve_type(len=:)), allocatable :: sieve
  logical, pointer :: check(:) => null()
  integer(int64) :: i, sln

  allocate (sieve_type(len=n) :: sieve)
  call sift(sieve, check=check)
  sln = 0
  do i = 1, n
    if (check(i)) sln = sln + i
  end do
  write (problem%answer, "(i20)") sln
  nullify (check)
end subroutine euler0010

end submodule submodule_euler0010

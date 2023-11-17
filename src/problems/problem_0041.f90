submodule(module_problem) submodule_euler0041
implicit none
contains

module subroutine euler0041(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 7654321
  type(sieve_type(len=:)), allocatable :: sieve
  logical, pointer :: check(:) => null()
  integer(int64) :: i, sln

  allocate (sieve_type(len=n) :: sieve)
  call sift(sieve, check=check)
  do i = n, 1, -1
    if (check(i) .and. is_pandigital(i)) then
      sln = i
      exit
    end if
  end do
  write (problem%answer, "(i20)") sln
  nullify (check)
end subroutine euler0041

end submodule submodule_euler0041

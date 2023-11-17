submodule(module_problem) submodule_euler0026
implicit none
contains

module subroutine euler0026(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 1000
  type(sieve_type(len=:)), allocatable :: sieve
  logical, pointer :: check(:) => null()
  integer(int64) :: k, i, sln, tmp

  allocate (sieve_type(len=1000) :: sieve)
  call sift(sieve, check=check)
  k = 1
  do i = 7, n
    if (check(i)) then
      tmp = multiplicative_order(i, 10_int64)
      if (tmp > k) then
        k = tmp
        sln = i
      end if
    end if
  end do
  write (problem%answer, "(i20)") sln
  nullify (check)
end subroutine euler0026

pure function multiplicative_order(n, m) result(ret)
  integer(int64), intent(in) :: n, m
  integer(int64) :: k, tmp
  integer(int64) :: ret

  ret = 1
  k = 1
  do
    tmp = mod(k*m, n)
    if (tmp /= 1) then
      k = tmp
      ret = ret + 1
    else
      exit
    end if
  end do
end function multiplicative_order

end submodule submodule_euler0026

submodule(module_problem) submodule_euler0026
implicit none
contains

module subroutine euler0026(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 1000
  logical, allocatable :: check(:)
  integer(int64) :: k, i, sln, tmp

  check = sift(n)
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

submodule(module_interface) submodule_euler0021
implicit none
contains

module subroutine euler0021(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32), parameter :: n = 10000
  integer(int32) :: i, sln

  sln = 0
  do i = 1, n
    if (sum_divisors(sum_divisors(i)) == i .and. &
      & sum_divisors(i) /= i) sln = sln + i
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0021

elemental function sum_divisors(n) result(ret)
  integer(int32), intent(in) :: n
  integer(int32) :: ret
  integer(int32) :: i

  ret = 1
  do i = 2, sqrt(n)
    if (mod(n, i) == 0) then
      ret = merge(ret + i, ret + i + n/i, n/i == i)
    end if
  end do
end function sum_divisors

end submodule submodule_euler0021
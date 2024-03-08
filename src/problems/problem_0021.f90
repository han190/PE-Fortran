submodule(module_problem) submodule_euler0021
implicit none
contains

module subroutine euler0021(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: n = 10000
  integer(int64) :: i, sln

  sln = 0
  do i = 1, n
    if (sum_divisors(sum_divisors(i)) == i .and. &
      & sum_divisors(i) /= i) sln = sln + i
  end do
  write (answer, "(i20)") sln
end subroutine euler0021

elemental function sum_divisors(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: ret
  integer(int64) :: i

  ret = 1
  do i = 2, sqrt(n)
    if (mod(n, i) == 0) then
      ret = merge(ret + i, ret + i + n/i, n/i == i)
    end if
  end do
end function sum_divisors

end submodule submodule_euler0021
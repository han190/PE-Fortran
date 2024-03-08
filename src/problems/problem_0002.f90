submodule(module_problem) submodule_euler0002
implicit none
contains

module subroutine euler0002(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: n = 4000000
  integer(int64) :: i, sln

  i = 0
  sln = 0
  do while (fibonacci(i) <= n)
    sln = sln + fibonacci(i)
    i = i + 3
  end do
  write (answer, "(i20)") sln
end subroutine euler0002

elemental recursive function fibonacci(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: k, ret

  if (n == 0) then
    ret = 0
  else if (any(n == [1, 2])) then
    ret = 1
  else if (mod(n, 2_int64) == 0) then
    k = n/2
    ret = fibonacci(k)*(fibonacci(k + 1)*2 - fibonacci(k))
  else
    k = (n - 1)/2
    ret = fibonacci(k + 1)**2 + fibonacci(k)**2
  end if
end function fibonacci

end submodule submodule_euler0002

submodule(module_problem) submodule_euler0001
implicit none
contains

module subroutine euler0001(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64) :: i, j, n, sln

  i = 3
  j = 5
  n = 1000
  sln = sdb(i, n - 1) + sdb(j, n - 1) - sdb(i*j, n - 1)
  write (answer, "(i20)") sln
end subroutine euler0001

elemental function sdb(i, j) result(ret)
  integer(int64), intent(in) :: i, j
  integer(int64) :: ret

  ret = i*(j/i*(j/i + 1))/2
end function sdb

end submodule submodule_euler0001

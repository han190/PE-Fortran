submodule(module_problem) submodule_euler0005
implicit none
contains

module subroutine euler0005(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: n = 20
  integer(int64) :: i, sln

  sln = 1
  do i = 1, n
    sln = lcm(sln, i)
  end do
  write (answer, "(i20)") sln
end subroutine euler0005

end submodule submodule_euler0005

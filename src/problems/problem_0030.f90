submodule(module_problem) submodule_euler0030
implicit none
contains

module subroutine euler0030(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: n = 999999
  integer(int64) :: i, sln

  sln = 0
  do i = 2, n
    if (sum(to_array(i)**5) == i) sln = sln + i
  end do
  write (answer, "(i20)") sln
end subroutine euler0030

end submodule submodule_euler0030

submodule(module_problem) submodule_euler0006
implicit none
contains

module subroutine euler0006(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: n = 100
  integer(int64) :: i, sln

  associate (array => [(i, i=1, n)])
    sln = sum(array)**2 - sum(array**2)
  end associate
  write (answer, "(i20)") sln
end subroutine euler0006

end submodule submodule_euler0006

submodule(module_problem) submodule_euler0004
implicit none
contains

module subroutine euler0004(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: start = 100, end = 999
  integer(int64) :: i, j, sln

  sln = 0
  do i = start, end
    do j = start, end
      if (is_palindromic(i*j) .and. i*j > sln) sln = i*j
    end do
  end do
  write (answer, "(i20)") sln
end subroutine euler0004

end submodule submodule_euler0004

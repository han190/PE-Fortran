submodule(module_problem) submodule_euler0036
implicit none
contains

module subroutine euler0036(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: n = 1000000
  integer(int64) :: i, sln

  sln = 0
  do i = 1, n, 2 ! A base-2 number must be odd to be a palindrome.
    if (is_palindromic(i, 2_int64) .and. &
      & is_palindromic(i)) sln = sln + i
  end do
  write (answer, "(i20)") sln
end subroutine euler0036

end submodule submodule_euler0036

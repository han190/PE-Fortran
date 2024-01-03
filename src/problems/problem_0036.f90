submodule(module_problem) submodule_euler0036
implicit none
contains

module subroutine euler0036(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 1000000
  integer(int64) :: i, sln

  sln = 0
  do i = 1, n, 2 ! A base-2 number must be odd to be a palindrome.
    if (is_palindromic2(i, 10_int64) .and. &
      & is_palindromic2(i, 2_int64)) sln = sln + i
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0036

pure logical function is_palindromic2(n, base)
  integer(int64), intent(in) :: n, base
  integer(int64) :: reversed, tmp

  reversed = 0
  tmp = n
  do while (tmp > 0)
    reversed = base*reversed + mod(tmp, base) 
    tmp = tmp/base
  end do
  is_palindromic2 = n == reversed
end function is_palindromic2

end submodule submodule_euler0036

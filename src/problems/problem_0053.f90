submodule(module_interface) submodule_euler0053
implicit none
contains

module subroutine euler0053(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32), parameter :: upper = 100, b = 1000000
  integer(int32) :: n, r, c(0:upper, 0:upper), sln

  sln = 0
  do n = 1, upper
    c(n, 0) = 1
    c(n, n) = 1
    do r = 1, n - 1
      c(n, r) = c(n - 1, r - 1) + c(n - 1, r)
      if (c(n, r) >= b) then
        sln = sln + 1
        c(n, r) = b
      end if
    end do
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0053

end submodule submodule_euler0053

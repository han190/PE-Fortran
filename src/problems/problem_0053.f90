submodule(module_problem) submodule_euler0053
implicit none
contains

module subroutine euler0053(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: upper = 100, b = 1000000
  integer(int64), allocatable :: c(:, :)
  integer(int64) :: n, r, sln

  allocate (c(0:upper, 0:upper))
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
  write (answer, "(i20)") sln
end subroutine euler0053

end submodule submodule_euler0053

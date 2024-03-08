submodule(module_problem) submodule_euler0020
implicit none
contains

module subroutine euler0020(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), allocatable :: long_integer(:)
  integer(int64) :: i
  real(real64) :: tmp

  tmp = 0.0
  do i = 1, 100
    tmp = tmp + log10(real(i))
  end do
  allocate (long_integer(floor(tmp + 1)))

  long_integer = 0
  long_integer(size(long_integer)) = 1

  do i = 1, 100
    call multiply(long_integer, i)
  end do
 write (answer, "(i20)") sum(long_integer)
end subroutine euler0020

pure subroutine multiply(long_integer, n)
  integer(int64), contiguous, intent(inout) :: long_integer(:)
  integer(int64), intent(in) :: n

  long_integer = long_integer*n
  call carry(long_integer)
end subroutine multiply

end submodule submodule_euler0020
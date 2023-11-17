submodule(module_interface) submodule_euler0020
implicit none
contains

module subroutine euler0020(problem)
  type(problem_type), intent(inout) :: problem
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
 write (problem%answer, "(i20)") sum(long_integer)
end subroutine euler0020

pure subroutine multiply(long_integer, n)
  integer(int64), contiguous, intent(inout) :: long_integer(:)
  integer(int64), intent(in) :: n

  associate (tmp => carry(long_integer*n))
    long_integer = tmp(2:)
  end associate
end subroutine multiply

end submodule submodule_euler0020
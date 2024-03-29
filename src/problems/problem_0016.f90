submodule(module_problem) submodule_euler0016
implicit none
contains

module subroutine euler0016(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), allocatable :: long_integer(:)
  integer(int64) :: i

  allocate (long_integer(floor(1000*log10(2.0)) + 1))
  long_integer = 0
  long_integer(size(long_integer)) = 2

  do i = 2, 1000
    call multiply_2(long_integer)
  end do
  write (problem%answer, "(i20)") sum(long_integer)
end subroutine euler0016

pure subroutine multiply_2(long_integer)
  integer(int64), contiguous, intent(inout) :: long_integer(:)

  long_integer = long_integer*2
  call carry(long_integer)
end subroutine multiply_2

end submodule submodule_euler0016

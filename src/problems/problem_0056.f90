submodule(module_problem) submodule_euler0056
implicit none
contains

module subroutine euler0056(problem)
  type(problem_type), intent(inout) :: problem
  type(long_type(len=:)), allocatable :: a, x
  integer(int64) :: b, s, i

  allocate (long_type(len=200) :: a, x)
  s = 0
  do i = 90, 100
    a = to_array(i)
    do b = 90, 100
      x = a**b
      s = max(s, sum(x%digit))
    end do
  end do
  write (problem%answer, "(i0)") s
end subroutine euler0056

end submodule submodule_euler0056
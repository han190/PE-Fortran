submodule(module_problem) submodule_euler0056
implicit none
contains

module subroutine euler0056(problem)
  type(problem_type), intent(inout) :: problem
  type(long_type) :: a, x
  integer(int64) :: b, s, i

  call initialize(a, 200_int64)
  call initialize(x, 200_int64)
  s = 0
  do i = 90, 100
    a = to_array(i)
    do b = 90, 100
      x = a**b
      s = max(s, sum(x%digit))
    end do
  end do
  write (problem%answer, "(i20)") s
end subroutine euler0056

end submodule submodule_euler0056
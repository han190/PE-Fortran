submodule(interface_module) euler_problem_0034_submodule
implicit none

contains

module character(len=20) function euler0034()
  write (euler0034, "(i20)") answer()
end function euler0034

elemental integer(i32) function answer()
  integer(i32), parameter :: n = 40585
  integer(i32) :: i

  answer = sum([(i, i=1, n)], mask=[(is_curious(i), i=1, n)]) - 3
end function answer

elemental logical function is_curious(n)
  integer(i32), intent(in) :: n
  integer(i32) :: i

  is_curious = .false.
  associate (x => to_array(n))
    associate (tmp => sum([(factorial(x(i)), i=1, size(x))]))
      if (tmp == n) is_curious = .true.
    end associate
  end associate
end function is_curious

elemental integer(i32) function factorial(n)
  integer(i32), intent(in) :: n
  integer(i32) :: i

  factorial = product([(i, i=1, n)])
end function factorial

end submodule euler_problem_0034_submodule

submodule(interface_module) euler_problem_0063_submodule
implicit none

contains

module character(len=20) function euler0063()
  write (euler0063, "(i20)") answer()
end function euler0063

pure integer(i32) function answer()
  integer(i32) :: i, count_

  answer = 0
  do i = 1, 9
    associate (r => real(i, sp), log10 => log(10.))
      count_ = floor(log10/(log10 - log(r)))
      answer = answer + count_
    end associate
  end do
end function answer

end submodule euler_problem_0063_submodule

submodule(interface_module) euler_problem_0040_submodule
implicit none

contains

module character(len=20) function euler0040()
  write (euler0040, "(i20)") answer()
end function euler0040

elemental integer(i32) function answer()
  integer(i32) :: digit_array(6), digit(6)
  integer(i32) :: i

  digit_array = [0, 9, 189, 2889, 38889, 488889]
  digit = 1

  do i = 2, 6
    associate (x => floor(real((10**i - digit_array(i))/i), sp))
      digit(i) = x + 10**(i - 1) - 1
    end associate

    associate (x => to_array(digit(i)))
      digit(i) = x(mod(10**i - digit_array(i), i))
    end associate
  end do
  answer = product(digit)
end function answer

end submodule euler_problem_0040_submodule

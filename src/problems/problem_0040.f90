submodule(module_interface) submodule_euler0040
implicit none
contains

module subroutine euler0040(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: digit_array(6), digit(6)
  integer(int64) :: i

  digit_array = [0, 9, 189, 2889, 38889, 488889]
  digit = 1

  do i = 2, 6
    associate (x => floor(real((10**i - digit_array(i))/i), real64))
      digit(i) = x + 10**(i - 1) - 1
    end associate

    associate (x => to_array(digit(i)))
      digit(i) = x(mod(10**i - digit_array(i), i))
    end associate
  end do
  write (problem%answer, "(i20)") product(digit)
end subroutine euler0040

end submodule submodule_euler0040

submodule(interface_module) euler_problem_0052_submodule
implicit none

contains

module character(len=20) function euler0052()
  write (euler0052, "(i20)") answer()
end function euler0052

pure integer(i32) function answer()
  integer(i32) :: i, j
  logical :: array(0:9), array_temp(0:9)

  i = 1
  outer: do
    call digits_in_use(i, array)
    inner: do j = 2, 6
      call digits_in_use(i*j, array_temp)
      if (all(array .eqv. array_temp)) then
        if (j == 6) exit outer
      else
        exit inner
      end if
    end do inner
    i = i + 1
  end do outer
  answer = i
end function answer

pure subroutine digits_in_use(n, array)
  integer(i32), intent(in) :: n
  logical, intent(out) :: array(0:9)

  array = .false.
  array(to_array(n)) = .true.
end subroutine digits_in_use

end submodule euler_problem_0052_submodule

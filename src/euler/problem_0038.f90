submodule(interface_module) euler_problem_0038_submodule
implicit none

contains

module character(len=20) function euler0038()
  write (euler0038, "(i20)") answer()
end function euler0038

elemental integer(i64) function answer()
  integer(i64) :: i, j, tmp

  answer = 0
  outer: do i = 2, 10000
    j = 1; tmp = 0

    inner: do
      if (tmp /= 0) then
        if (number_of_digits(tmp) > 9) cycle outer
      end if

      if (is_pandigital(tmp)) then
        if (tmp > answer) answer = tmp
      end if

      tmp = i*j + tmp*10**number_of_digits(i*j)
      j = j + 1
    end do inner
  end do outer
end function answer

end submodule euler_problem_0038_submodule

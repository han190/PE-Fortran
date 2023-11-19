submodule(module_problem) submodule_euler0092
implicit none
integer(int64), parameter :: unhappy(8) = &
    & [4, 16, 37, 58, 89, 145, 42, 20]
contains

module subroutine euler0092(problem)
  class(problem_type), intent(inout) :: problem
  !> https://oeis.org/A068571
  !> https://en.wikipedia.org/wiki/Happy_number
  !> Find all 10-happy number number under 10**7
  write (problem%answer, "(i20)") 10**7 - num_happys(7_int64)
end subroutine euler0092

pure function num_happys(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: ret
  integer(int64) :: i, k

  ret = 1
  do i = 1, 9**2*n
    if (is_happy(i)) then
      ret = ret + h(i, n)
    end if
  end do
end function num_happys

recursive elemental function h(n, k) result(ret)
  integer(int64), intent(in) :: n, k
  integer(int64) :: ret
  integer(int64) :: i

  if (n == 0 .and. k == 0) then
    ret = 1
  else if (k == 0 .and. n > 0) then
    ret = 0
  else if (n < 0) then
    ret = 0
  else
    ret = 0
    do i = 0, 9
      ret = ret + h(n - i**2, k - 1)
    end do
  end if
end function h

pure logical function is_happy(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: tmp
  logical :: ret

  tmp = n
  ret = .true.
  do while (tmp /= 1)
    if (any(tmp == unhappy)) then
      ret = .false.
      exit
    end if
    tmp = sum_squares(tmp)
  end do
end function is_happy

pure function sum_squares(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: ret

  ret = sum(to_array(n)**2)
end function sum_squares

end submodule submodule_euler0092

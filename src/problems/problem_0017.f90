submodule(module_problem) submodule_euler0017
implicit none

integer(int64), parameter :: error = 9999999
contains

module subroutine euler0017(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: i

  write (problem%answer, "(i20)") sum([(count_letters(i), i=1, 1000)])
end subroutine euler0017

elemental function count_letters(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: ret

  select case (n)
  case (1:9)
    ret = count_letters_1(n)
  case (10:99)
    ret = count_letters_2(n)
  case (100:999)
    ret = count_letters_3(n)
  case (1000)
    ret = 11
  case default
    error stop "[P17] Not supported number."
  end select
end function count_letters

elemental function count_letters_3(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: ret
  integer(int64) :: d1, d2

  associate (hundred => 7, and => 3)
    d1 = n/100
    ret = count_letters_1(d1) + hundred
    if (mod(n, 100) /= 0) then
      d2 = n - (n/100)*100
      ret = ret + and + count_letters_2(d2)
    end if
  end associate
end function count_letters_3

elemental function count_letters_2(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: ret
  integer(int64) :: d1, d2

  d1 = 0
  d2 = 0
  if (count_letters_1(n) /= error) then
    ret = count_letters_1(n)
  else
    d1 = (n/10)*10
    d2 = n - d1
    ret = count_letters_1(d1) + count_letters_1(d2)
  end if

  if (n == 0) ret = 0
end function count_letters_2

elemental function count_letters_1(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: ret

  select case (n)
  case (0)
    ret = 0
  case (1, 2, 6, 10)
    ret = 3
  case (4, 5, 9)
    ret = 4
  case (3, 7, 8, 40, 50, 60)
    ret = 5
  case (11, 12, 20, 30, 80, 90)
    ret = 6
  case (15, 16, 70)
    ret = 7
  case (13, 14, 18, 19)
    ret = 8
  case (17)
    ret = 9
  case (1000)
    ret = 8
  case default
    ret = error
  end select
end function count_letters_1

end submodule submodule_euler0017
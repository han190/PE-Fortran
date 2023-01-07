submodule(interface_module) euler_problem_0017_submodule
implicit none

contains

module character(len=20) function euler0017()
  write (euler0017, "(i20)") answer()
end function euler0017

elemental integer(i32) function answer()
  integer(i32) :: i

  answer = sum([(count_letters(i), i=1, 1000)])
end function answer

elemental function count_letters(n) result(ret)
  integer(i32), intent(in) :: n
  integer(i32) :: ret

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
    error stop "Not supported number."
  end select
end function count_letters

elemental function count_letters_3(n) result(ret)
  integer(i32), intent(in) :: n
  integer(i32) :: ret
  integer(i32) :: d1, d2

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
  integer(i32), intent(in) :: n
  integer(i32) :: ret
  integer(i32) :: d1, d2

  d1 = 0; d2 = 0
  if (count_letters_1(n) /= 1000) then
    ret = count_letters_1(n)
  else
    d1 = (n/10)*10
    d2 = n - d1
    ret = count_letters_1(d1) + count_letters_1(d2)
  end if

  if (n == 0) ret = 0
end function count_letters_2

elemental function count_letters_1(n) result(ret)
  integer(i32), intent(in) :: n
  integer(i32) :: ret

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
    ret = 1000
  end select
end function count_letters_1

end submodule euler_problem_0017_submodule

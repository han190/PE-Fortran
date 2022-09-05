submodule(interface_module) euler_problem_0055_submodule
  use multiprecision_module
  implicit none

contains

  module character(len=20) function euler0055()
    write (euler0055, "(i20)") answer()
  end function euler0055

  elemental integer(i32) function answer()
    integer(i32), parameter :: upper_ = 10000
    integer(i32) :: i

    answer = 0
    do i = 1, upper_
      if (is_lychrel(int(i, "ll"))) answer = answer + 1
    end do
  end function answer

  elemental logical function is_lychrel(n)
    type(long_integer), intent(in) :: n
    type(long_integer) :: temp, temp2
    integer(i32) :: i

    i = 0; temp = n
    is_lychrel = .true.

    do while (i <= 50)
      temp2 = reverse(temp) + temp
      if (is_palindromic_(temp2)) then
        is_lychrel = .false.
        return
      end if
      temp = temp2
      i = i + 1
    end do
  end function is_lychrel

  elemental logical function is_palindromic_(val)
    type(long_integer), intent(in) :: val

    if (reverse(val) == val) then
      is_palindromic_ = .true.
    else
      is_palindromic_ = .false.
    end if
  end function is_palindromic_

  elemental function reverse(val) result(ret)
    type(long_integer), intent(in) :: val
    type(long_integer) :: ret

    associate (n => len(val))
      allocate (ret%digit(n))
      ret%digit = val%digit(n:1:-1)
      ret%sign = val%sign
    end associate
  end function reverse

end submodule euler_problem_0055_submodule

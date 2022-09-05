submodule(interface_module) euler_problem_0019_submodule
  implicit none

contains

  module character(len=20) function euler0019()
    write (euler0019, "(i20)") answer()
  end function euler0019

  elemental integer(i32) function answer()
    integer(i32), parameter :: start_year = 1901, end_year = 2000
    integer(i32) :: day_of_month(12), day_of_week
    integer(i32) :: year, month

    day_of_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    answer = 0
    day_of_week = 1

    outer: do year = start_year, end_year
      if (is_leap(year)) day_of_month(2) = 29

      inner: do month = 1, 12
        day_of_week = day_of_week + mod(day_of_month(month), 7) - 1
        if (mod(day_of_week, 7) == 0) answer = answer + 1
      end do inner
    end do outer
  end function answer

  elemental logical function is_leap(year)
    integer(i32), intent(in) :: year

    is_leap = .false.
    if ((mod(year, 4) == 0 .and. mod(year, 100) /= 0) .or. &
        mod(year, 400) == 0) is_leap = .false.
  end function is_leap

end submodule euler_problem_0019_submodule

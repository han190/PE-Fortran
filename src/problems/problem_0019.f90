submodule(module_problem) submodule_euler0019
implicit none
contains

module subroutine euler0019(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: num_days(12), year, month, day_of_week, sln

  num_days = 30 + [1, -2, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1]
  sln = 0
  day_of_week = 1

  do year = 1901, 2000
    num_days(2) = merge(29, 28, is_leap(year))
    do month = 1, 12
      day_of_week = day_of_week + mod(num_days(month), 7) - 1
      if (mod(day_of_week, 7) == 0) sln = sln + 1
    end do
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0019

elemental logical function is_leap(year)
  integer(int64), intent(in) :: year

  is_leap = (mod(year, 4) == 0 .and. mod(year, 100) /= 0) .or. mod(year, 400) == 0
end function is_leap

end submodule submodule_euler0019

submodule(module_problem) submodule_euler0064
implicit none
contains

module subroutine euler0064(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 1000_int64 ! Large enough
  integer(int64) :: i, p, sln

  sln = 0
  do i = 1, 10000
    if (i - sqrt(i)**2 > 0) then
      p = continued_fraction_period(i, n)
      sln = sln + merge(0, 1, mod(p, 2_int64) == 0)
    end if
  end do
  write (problem%answer, "(i0)") sln
end subroutine euler0064

pure function continued_fraction_period(n, num_sequence) result(period)
  integer(int64), intent(in) :: n, num_sequence
  integer(int64) :: period
  integer(int64) :: sequence(3, num_sequence), iterator(3)
  integer(int64) :: i, j, a0

  a0 = sqrt(n)
  iterator = [0_int64, 1_int64, a0]
  period = 1
  sequence(:, period) = iterator
  
  outer: do i = 2, num_sequence
    iterator(1) = iterator(2)*iterator(3) - iterator(1)
    iterator(2) = (n - iterator(1)*iterator(1))/iterator(2)
    iterator(3) = (a0 + iterator(1))/iterator(2)
    check_existence: do j = 1, period
      if (all(iterator == sequence(:, j))) exit outer
    end do check_existence
    period = period + 1
    sequence(:, period) = iterator
  end do outer
  period = period - 1
end function continued_fraction_period

end submodule submodule_euler0064
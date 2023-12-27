submodule(module_problem) submodule_euler0055
implicit none
contains

module subroutine euler0055(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: sln, i, j
  type(long_type) :: tmp, reversed

  !> Read the problem why I assigned 28 digits.
  call initialize(tmp, 28_int64)
  call initialize(reversed, 28_int64)
  sln = 0
  outer: do i = 1, 10000
    tmp = to_array(i)
    do j = 1, 50
      call reverse(tmp, reversed)
      tmp = tmp + reversed
      if (palindromic(tmp)) cycle outer
    end do
    sln = sln + 1
  end do outer
  write (problem%answer, "(i20)") sln
end subroutine euler0055

!> Check if a number if palindromic
pure function palindromic(x) result(ret)
  type(long_type), intent(in) :: x
  logical :: ret
  integer(int64) :: i, j, num_tests, num_digits
  integer(int64), allocatable :: digit(:)

  digit = x%digit(x%start:)
  num_digits = size(digit)
  num_tests = num_digits/2
  ret = .true.
  do i = 1, num_tests
    j = num_digits - i + 1
    if (digit(i) /= digit(j)) then
      ret = .false.
      exit
    end if
  end do
end function palindromic

!> UNO: reverse!
pure subroutine reverse(x, ret)
  type(long_type), intent(in) :: x
  type(long_type), intent(inout) :: ret

  associate (tmp => x%digit(x%start:))
    ret = tmp(size(tmp):1:-1)
  end associate
end subroutine reverse

end submodule submodule_euler0055

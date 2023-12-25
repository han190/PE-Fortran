submodule(module_problem) submodule_euler0055
implicit none
contains

module subroutine euler0055(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: sln, i

  sln = 0
  do i = 1, 10000
    if (is_lychrel(to_array(i))) sln = sln + 1
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0055

function is_lychrel(arr) result(ret)
  integer(int64), contiguous, intent(in) :: arr(:)
  logical :: ret
  integer(int64), allocatable :: tmp(:), reversed(:)
  integer(int64) :: i

  tmp = arr
  reversed = arr(size(arr):1:-1)
  ret = .true.

  do i = 1, 50
    tmp = add(tmp, reversed)
    if (is_palindromic_array(tmp, reversed)) then
      ret = .false.
      exit
    end if
  end do
end function is_lychrel

function is_palindromic_array(digit, reversed) result(ret)
  integer(int64), contiguous, intent(in) :: digit(:)
  integer(int64), allocatable, intent(out) :: reversed(:)
  logical :: ret
  integer(int64) :: i, j, num_tests, num_digits

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
  if (.not. ret) reversed = digit(num_digits:1:-1)
end function is_palindromic_array

pure function add(digit1, digit2) result(ret)
  integer(int64), contiguous, intent(in) :: digit1(:), digit2(:)
  integer(int64), allocatable :: ret(:)
  integer(int64), allocatable :: tmp1(:), tmp2(:)
  integer(int64) :: n, i

  associate (n1 => size(digit1), n2 => size(digit2))
    n = max(n1, n2) + 1
    tmp1 = [[(0_int64, i=1, n - n1)], digit1]
    tmp2 = [[(0_int64, i=1, n - n2)], digit2]
    tmp1 = tmp1 + tmp2
    call carry(tmp1)
    ret = cut_leading_zeros(tmp1)
  end associate
end function add

pure function cut_leading_zeros(digit) result(ret)
  integer(int64), contiguous, intent(in) :: digit(:)
  integer(int64), allocatable :: ret(:)
  integer(int64) :: i

  do i = 1, size(digit)
    if (digit(i) /= 0) then
      ret = digit(i:)
      exit
    end if
  end do
end function cut_leading_zeros

end submodule submodule_euler0055

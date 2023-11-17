submodule(module_interface) submodule_euler0055
implicit none
contains

module subroutine euler0055(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32) :: sln, i

  sln = 0
  do i = 1, 10000
    if (is_lychrel(to_array(i))) sln = sln + 1
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0055

function is_lychrel(arr) result(ret)
  integer(int32), contiguous, intent(in) :: arr(:)
  logical :: ret
  integer(int32), allocatable :: tmp(:), tmpr(:)
  integer(int32) :: i

  tmp = arr
  ret = .true.

  do i = 1, 50
    if (is_palindromic_array(tmp, tmpr)) then
      ret = .false.
      exit
    end if
    tmp = add(tmp, tmpr)
  end do
end function is_lychrel

function is_palindromic_array(digit, digitr) result(ret)
  integer(int32), contiguous, intent(in) :: digit(:)
  integer(int32), allocatable, intent(out) :: digitr(:)
  logical :: ret

  digitr = reverse(digit)
  ret = all(digitr == digit)
end function is_palindromic_array

pure function reverse(digit) result(ret)
  integer(int32), contiguous, intent(in) :: digit(:)
  integer(int32), allocatable :: ret(:)

  ret = digit(size(digit):1:-1)
end function reverse

pure function add(digit1, digit2) result(ret)
  integer(int32), contiguous, intent(in) :: digit1(:), digit2(:)
  integer(int32), allocatable :: ret(:)
  integer(int32), allocatable :: tmp1(:), tmp2(:)
  integer(int32) :: n, i

  associate (n1 => size(digit1), n2 => size(digit2))
    n = max(n1, n2)
    tmp1 = [[(0, i=1, n - n1)], digit1]
    tmp2 = [[(0, i=1, n - n2)], digit2]
    ret = cut_leading_zeros(carry(tmp1 + tmp2))
  end associate
end function add

pure function cut_leading_zeros(digit) result(ret)
  integer(int32), contiguous, intent(in) :: digit(:)
  integer(int32), allocatable :: ret(:)
  integer(int32) :: i

  do i = 1, size(digit)
    if (digit(i) /= 0) then
      ret = digit(i:)
      return
    end if
  end do
end function cut_leading_zeros

end submodule submodule_euler0055

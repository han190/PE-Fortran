submodule(module_problem) submodule_euler0092
implicit none
!> https://oeis.org/A068571
!> https://en.wikipedia.org/wiki/Happy_number
!> Find all 10-happy number number under 10**7
integer(int64), parameter :: unhappy(8) = &
    & [4, 16, 37, 58, 89, 145, 42, 20]
integer(int64), parameter :: error = -9999999
contains

module subroutine euler0092(problem)
  type(problem_type), intent(inout) :: problem

  write (problem%answer, "(i20)") 10_int64**7 - num_happys(7_int64)
end subroutine euler0092

function num_happys(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: ret
  integer(int64) :: i
  integer(int64), allocatable :: arr(:,:)

  !> An array to store computed results
  allocate (arr(0:9**2*n, 0:n))
  arr = error
  ret = 1
  do i = 1, 9**2*n
    if (is_happy(i)) then
      ret = ret + h(i, n, arr)
    end if
  end do
end function num_happys

recursive function h(n, k, arr) result(ret)
  integer(int64), intent(in) :: n, k
  integer(int64), intent(inout) :: arr(:,:)
  integer(int64) :: ret
  integer(int64) :: i, a, np, kp

  if (within([n, k], arr)) then
    if (arr(n, k) /= error) then
      ret = arr(n, k)
      return
    end if
  end if

  if (n < 0) then
    ret = 0
  else if (k == 0) then
    ret = merge(1, 0, n == 0)
  else
    ret = 0
    do i = 0, 9
      np = n - i**2
      kp = k - 1
      if (within([np, kp], arr)) then
        if (arr(np, kp) /= error) then
          a = arr(np, kp)
        else
          a = h(np, kp, arr)
        end if
      else
        a = h(np, kp, arr)
      end if
      ret = ret + a
    end do
  end if

  if (within([n, k], arr)) then
    if (arr(n, k) == error) arr(n, k) = ret
  end if
end function h

pure function within(indices, array) result(ret)
  integer(int64), intent(in) :: indices(:), array(:, :)
  logical :: ret
  integer(int64) :: l(2), u(2), i

  do i = 1, 2
    l(i) = lbound(array, dim=i)
    u(i) = ubound(array, dim=i)
  end do
  ret = all(indices >= l) .and. all(indices <= u)
end function within

elemental function is_happy(n) result(ret)
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

elemental function sum_squares(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64) :: ret

  ret = sum(to_array(n)**2)
end function sum_squares

end submodule submodule_euler0092

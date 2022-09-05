submodule(interface_module) euler_problem_0048_submodule
  ! use multiprecision_m
  implicit none

contains

  module character(len=20) function euler0048()
    write (euler0048, "(a20)") trim(answer())
  end function euler0048

  pure character(len=20) function answer()
    integer(i32) :: sum_(10), i, array(10)

    sum_ = 0
    do i = 1, 1000
      call self_pow10(i, array)
      call add10(sum_, array)
    end do
    write (answer, "(20(i1))") sum_
  end function answer

  pure subroutine add10(a, b)
    integer(i32), intent(inout) :: a(:)
    integer(i32), intent(in) :: b(:)
    integer(i32) :: i

    a = a + b
    do i = 10, 2, -1
      call carry(a(i - 1:i), 1)
    end do
    a(1) = unit_digit(a(1))
  end subroutine add10

  pure subroutine self_pow10(n, array)
    integer(i32), intent(in) :: n
    integer(i32), intent(out) :: array(:)
    integer(i32) :: i, j

    array = 0; array(10) = n
    do i = 1, n - 1
      array(10) = array(10)*n
      do j = 10, 2, -1
        call carry(array(j - 1:j), n)
      end do
      array(1) = unit_digit(array(1))
    end do
  end subroutine self_pow10

  pure subroutine carry(a, m)
    integer(i32), intent(inout) :: a(:)
    integer(i32), intent(in) :: m
    integer(i32) :: tmp

    tmp = a(2)
    a(2) = unit_digit(tmp)
    a(1) = a(1)*m + (tmp - a(2))/10
  end subroutine carry

end submodule euler_problem_0048_submodule

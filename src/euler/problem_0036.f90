submodule(interface_module) euler_problem_0036_submodule
  implicit none

contains

  module character(len=20) function euler0036()
    write (euler0036, "(i20)") answer()
  end function euler0036

  pure integer(i32) function answer()
    integer(i32), parameter :: n = 1000000
    integer(i32) :: i

    answer = sum([(i, i=1, n)], mask=[(are_palindromics(i), i=1, n)])
  end function answer

  pure logical function are_palindromics(n)
    integer(i32), intent(in) :: n

    if (.not. is_palindromic(n)) then
      are_palindromics = .false.
      return
    end if

    associate (x => to_binary(n))
      are_palindromics = all(x == x(size(x):1:-1))
    end associate
  end function are_palindromics

  pure function to_binary(n) result(ret)
    integer(i32), intent(in) :: n
    integer(i32), allocatable :: ret(:)
    integer(i32) :: i, tmp

    associate (length => floor(log2(n)) + 1)
      allocate (ret(length))
      tmp = n
      do i = 1, length
        ret(i) = mod(tmp, 2_i32)
        tmp = tmp/2_i32
      end do
    end associate
  end function to_binary

  elemental real(sp) function log2(n)
    integer(i32), intent(in) :: n

    log2 = log(real(n, sp))/log(2._sp)
  end function log2

end submodule euler_problem_0036_submodule

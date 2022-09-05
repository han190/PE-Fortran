submodule(interface_module) euler_problem_0032_submodule
  implicit none

contains

  module character(len=20) function euler0032()
    write (euler0032, "(i20)") answer()
  end function euler0032

  pure integer(i64) function answer()
    integer(i64) :: tmp, i, j, k, products(9)

    k = 1
    do i = 1, 9
      do j = 1234, 9876
        if (number_of_digits(i*j) > 4) cycle
        tmp = i*10**8 + j*10**4 + i*j

        if (is_pandigital(tmp)) then
          products(k) = i*j
          k = k + 1
        end if
      end do
    end do

    do i = 12, 98
      do j = 123, 987
        if (number_of_digits(i*j) > 4) cycle
        tmp = i*10**7 + j*10**4 + i*j

        if (is_pandigital(tmp)) then
          products(k) = i*j
          k = k + 1
        end if
      end do
    end do

    answer = sum(remove_duplicates(products))
  end function answer

  pure function remove_duplicates(input) result(ret)
    integer(i64), intent(in) :: input(:)
    integer(i64), allocatable :: ret(:)
    integer(i64) :: i, k, tmp(size(input))

    tmp = 0
    k = 1
    tmp(1) = input(1)

    do i = 2, size(input)
      if (any(tmp == input(i))) cycle
      k = k + 1
      tmp(k) = input(i)
    end do

    ret = tmp(1:k)
  end function remove_duplicates

end submodule euler_problem_0032_submodule

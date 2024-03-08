submodule(module_problem) submodule_euler0032
implicit none
contains

module subroutine euler0032(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64) :: tmp, i, j, k, products(9)

  k = 1
  do i = 1, 9
    do j = 1234, 9876
      if (num_digits(i*j) > 4) cycle
      tmp = i*10**8 + j*10**4 + i*j

      if (is_pandigital(tmp)) then
        products(k) = i*j
        k = k + 1
      end if
    end do
  end do

  do i = 12, 98
    do j = 123, 987
      if (num_digits(i*j) > 4) cycle
      tmp = i*10**7 + j*10**4 + i*j

      if (is_pandigital(tmp)) then
        products(k) = i*j
        k = k + 1
      end if
    end do
  end do

  write (answer, "(i20)") sum(remove_duplicates(products))
end subroutine euler0032

pure function remove_duplicates(input) result(ret)
  integer(int64), intent(in) :: input(:)
  integer(int64), allocatable :: ret(:)
  integer(int64) :: i, k, tmp(size(input))

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

end submodule submodule_euler0032
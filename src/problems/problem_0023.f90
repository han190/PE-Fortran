submodule(module_interface) submodule_euler0023
implicit none
contains

module subroutine euler0023(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32), allocatable :: abundants(:)
  integer(int32), parameter :: istart = 12, iend = 28123
  integer(int32) :: i, j
  logical, allocatable :: sum_abundants(:)

  abundants = pack([(i, i=istart, iend)], [(is_abundant(i), i=istart, iend)])
  allocate (sum_abundants(iend))
  sum_abundants = .false.

  do i = 1, size(abundants)
    do j = i, size(abundants)
      if (abundants(i) + abundants(j) <= iend) &
        & sum_abundants(abundants(i) + abundants(j)) = .true.
    end do
  end do
  write (problem%answer, "(i20)") sum([(i, i=1, iend)],.not. sum_abundants)
end subroutine euler0023

elemental logical function is_abundant(n)
  integer(int32), intent(in) :: n

  is_abundant = sum_divisors(n) > n
end function is_abundant

elemental function sum_divisors(n) result(ret)
  integer(int32), intent(in) :: n
  integer(int32) :: ret
  integer(int32) :: i

  ret = 1
  do i = 2, sqrt(n)
    if (mod(n, i) == 0) then
      ret = merge(ret + i, ret + i + n/i, n/i == i)
    end if
  end do
end function sum_divisors

end submodule submodule_euler0023

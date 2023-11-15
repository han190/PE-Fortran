submodule(module_interface) submodule_euler0048
implicit none
contains

module subroutine euler0048(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32) :: sln(10), i, array(10)

  sln = 0
  do i = 1, 1000
    call self_pow10(i, array)
    call add10(sln, array)
  end do
  write (problem%answer, "(20(i1))") sln
end subroutine euler0048

pure subroutine add10(a, b)
  integer(int32), intent(inout) :: a(:)
  integer(int32), intent(in) :: b(:)
  integer(int32) :: i

  a = a + b
  do i = 10, 2, -1
    call carry2digits(a(i - 1:i), 1)
  end do
  a(1) = unit_digit(a(1))
end subroutine add10

pure subroutine self_pow10(n, array)
  integer(int32), intent(in) :: n
  integer(int32), intent(out) :: array(:)
  integer(int32) :: i, j

  array = 0
  array(10) = n
  do i = 1, n - 1
    array(10) = array(10)*n
    do j = 10, 2, -1
      call carry2digits(array(j - 1:j), n)
    end do
    array(1) = unit_digit(array(1))
  end do
end subroutine self_pow10

pure subroutine carry2digits(a, m)
  integer(int32), intent(inout) :: a(:)
  integer(int32), intent(in) :: m
  integer(int32) :: tmp

  tmp = a(2)
  a(2) = unit_digit(tmp)
  a(1) = a(1)*m + (tmp - a(2))/10
end subroutine carry2digits

end submodule submodule_euler0048

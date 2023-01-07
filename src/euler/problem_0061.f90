submodule(interface_module) euler_problem_0061_submodule
implicit none

contains

module character(len=20) function euler0061()
  write (euler0061, "(i20)") answer()
end function euler0061

integer(i32) function answer()
  type(array_type) :: polygonals(3:8)
  type(array_type), allocatable :: array(:)
  integer(i32) :: i(6), k

  answer = 0
  i = [3, 4, 5, 6, 7, 8]
  call get_polygonals(polygonals)

  outer: do
    do k = 1, 6
      call get_cyclic(array, polygonals(i(k))%array)
    end do

    if (allocated(array)) then
      inner: do k = 1, size(array)
        if (size(array(k)%array) /= 6) cycle inner
        if (ouroboric(array(k)%array)) exit outer
      end do inner
    end if

    if (.not. next_permute(i)) exit outer
    if (allocated(array)) deallocate (array)
  end do outer
  answer = sum(array(k)%array)

contains

  pure logical function ouroboric(x)
    integer(i32), intent(in) :: x(:)

    ouroboric = .false.
    if (x(1)/100 == mod(x(size(x)), 100)) ouroboric = .true.
  end function ouroboric

end function answer

pure subroutine get_cyclic(x, y)
  type(array_type), allocatable, intent(inout) :: x(:)
  integer(i32), intent(in) :: y(:)
  type(array_type) :: tmp(99)
  integer(i32) :: i, j, k, s

  if (.not. allocated(x)) then
    allocate (x(size(y)))
    do i = 1, size(x)
      x(i)%array = [y(i)]
    end do
    return
  end if

  k = 0
  do i = 1, size(x)
    s = size(x(i)%array)
    do j = 1, size(y)
      if (mod(x(i)%array(s), 100) == y(j)/100) then
        k = k + 1
        tmp(k)%array = [x(i)%array, [y(j)]]
      end if
    end do
  end do

  deallocate (x)
  if (k > 0) then
    allocate (x(k))
    do i = 1, k
      x(i)%array = tmp(i)%array
    end do
  end if
end subroutine get_cyclic

pure subroutine get_polygonals(p)
  type(array_type), intent(out) :: p(3:8)
  integer(i32) :: i, j
  integer(i32), parameter :: i_ = 1000, f_ = 9999

  do i = 3, 8
    associate (mask_ => ([(is_polygonal(i, j), j=i_, f_)]))
      p(i)%array = pack([(j, j=i_, f_)], mask_)
    end associate
  end do
end subroutine get_polygonals

pure function is_polygonal(n, val) result(ret)
  integer, intent(in) :: n
  integer, intent(in) :: val
  logical :: ret
  real :: x, c(4)

  select case (n)
    !> triangular
  case (3)
    c = [1., 8., -1., 2.]
    !> square
  case (4)
    c = [0., 1., 0., 1.]
    !> pentagonal
  case (5)
    c = [1., 24., 1., 6.]
    !> hexaonal
  case (6)
    c = [1., 8., 1., 4.]
    !> heptagonal
  case (7)
    c = [9., 40., 3., 10.]
    !> octagonal
  case (8)
    c = [1., 3., 1., 3.]
  case default
    error stop "Invalid poly!"
  end select

  associate (x_ => real(val))
    x = (sqrt(c(1) + c(2)*x_) + c(3))/c(4)
    ret = is_int(x)
  end associate
end function is_polygonal

elemental logical function is_int(val)
  real, intent(in) :: val
  real, parameter :: eps = tiny(0.)

  is_int = abs(val - nint(val)) < eps
end function is_int

end submodule euler_problem_0061_submodule

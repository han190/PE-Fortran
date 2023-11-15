submodule(module_interface) submodule_euler0061
implicit none
contains

module subroutine euler0061(problem)
  type(problem_type), intent(inout) :: problem
  type(jagged_type) :: polygonals(3:8)
  type(jagged_type), allocatable :: array(:)
  type(permutation_type(n=6, k=6)) :: permutation
  integer(int32) :: i, ordered(6)

  ordered = [(i, i=3, 8)]
  call initialize(permutation)
  call get_polygonals(polygonals)

  outer: do while (permutable(permutation))
    do i = 1, 6
      associate (indices => ordered(permutation%indices))
        call get_cyclic(array, polygonals(indices(i))%array)
      end associate
    end do

    if (allocated(array)) then
      inner: do i = 1, size(array)
        if (size(array(i)%array) /= 6) cycle inner
        if (ouroboric(array(i)%array)) exit outer
      end do inner
    end if
    if (allocated(array)) deallocate (array)
  end do outer
  write (problem%answer, "(i20)") sum(array(i)%array)
end subroutine euler0061

pure logical function ouroboric(x)
  integer(int32), intent(in) :: x(:)

  ouroboric = x(1)/100 == mod(x(size(x)), 100)
end function ouroboric

pure subroutine get_cyclic(x, y)
  type(jagged_type), allocatable, intent(inout) :: x(:)
  integer(int32), intent(in) :: y(:)
  type(jagged_type) :: tmp(99)
  integer(int32) :: i, j, k, s

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
  type(jagged_type), intent(out) :: p(3:8)
  integer(int32) :: i, j
  integer(int32), parameter :: i_ = 1000, f_ = 9999

  do i = 3, 8
    associate (mask_ => ([(is_polygonal(i, j), j=i_, f_)]))
      p(i)%array = pack([(j, j=i_, f_)], mask_)
    end associate
  end do
end subroutine get_polygonals

pure function is_polygonal(n, val) result(ret)
  integer, intent(in) :: n, val
  logical :: ret

  select case (n)
  case (3)
    ret = is_triangle(val)
  case (4)
    ret = is_square(val)
  case (5)
    ret = is_pentagonal(val)
  case (6)
    ret = is_hexaonal(val)
  case (7)
    ret = is_heptagonal(val)
  case (8)
    ret = is_octagonal(val)
  end select
end function is_polygonal

pure function is_triangle(val) result(ret)
  integer, intent(in) :: val
  logical :: ret

  ret = .false.
  if (is_int(.5*(sqrt(1.+8.*real(val)) - 1.))) ret = .true.
end function is_triangle

pure function is_square(val) result(ret)
  integer, intent(in) :: val
  logical :: ret

  ret = .false.
  if (is_int(sqrt(real(val)))) ret = .true.
end function is_square

pure function is_pentagonal(val) result(ret)
  integer, intent(in) :: val
  logical :: ret

  ret = .false.
  if (is_int((sqrt(1.+24.*real(val)) + 1.)/6.)) ret = .true.
end function is_pentagonal

pure function is_hexaonal(val) result(ret)
  integer, intent(in) :: val
  logical :: ret

  ret = .false.
  if (is_int(.25*(sqrt(1.+8.*real(val)) + 1.))) ret = .true.
end function is_hexaonal

pure function is_heptagonal(val) result(ret)
  integer, intent(in) :: val
  logical :: ret

  ret = .false.
  if (is_int((sqrt(9.+40.*real(val)) + 3.)/10.)) ret = .true.
end function is_heptagonal

pure function is_octagonal(val) result(ret)
  integer, intent(in) :: val
  logical :: ret

  ret = .false.
  if (is_int((sqrt(1.+3.*real(val)) + 1.)/3.)) ret = .true.
end function is_octagonal

pure function is_int(val) result(ret)
  real, intent(in) :: val
  logical :: ret
  real, parameter :: eps = tiny(0.)

  ret = .false.
  if (abs(val - nint(val)) < eps) ret = .true.
end function is_int

end submodule submodule_euler0061

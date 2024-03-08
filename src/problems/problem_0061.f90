submodule(module_problem) submodule_euler0061
implicit none
real, parameter :: eps = tiny(0.0)
contains

module subroutine euler0061(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  type(jagged_type) :: polygonals(3:8)
  type(jagged_type), allocatable :: array(:)
  type(permutation_type) :: permutation
  integer(int64) :: i, ordered(6)

  ordered = [(i, i=3, 8)]
  permutation = new_permutation(n=6_int64)
  call get_polygonals(polygonals)

  outer: do while (permutable(permutation))
    do i = 1, 6
      associate (indices => ordered(index(permutation)))
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
  write (answer, "(i20)") sum(array(i)%array)
end subroutine euler0061

pure logical function ouroboric(x)
  integer(int64), intent(in) :: x(:)

  ouroboric = x(1)/100_int64 == mod(x(size(x)), 100_int64)
end function ouroboric

pure subroutine get_cyclic(x, y)
  type(jagged_type), allocatable, intent(inout) :: x(:)
  integer(int64), intent(in) :: y(:)
  type(jagged_type) :: tmp(99)
  integer(int64) :: i, j, k, s

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
      if (mod(x(i)%array(s), 100_int64) == y(j)/100) then
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
  integer(int64) :: i, j
  integer(int64), parameter :: a = 1000, b = 9999

  do i = 3, 8
    associate (mask => [(is_polygonal(i, j), j=a, b)])
      p(i)%array = pack([(j, j=a, b)], mask)
    end associate
  end do
end subroutine get_polygonals

pure function is_polygonal(n, val) result(ret)
  integer(int64), intent(in) :: n, val
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

pure logical function is_triangle(val)
  integer(int64), intent(in) :: val

  is_triangle = is_integer(.5*(sqrt(1.+8.*real(val)) - 1.))
end function is_triangle

pure logical function is_square(val)
  integer(int64), intent(in) :: val

  is_square = is_integer(sqrt(real(val)))
end function is_square

pure logical function is_pentagonal(val)
  integer(int64), intent(in) :: val

  is_pentagonal = is_integer((sqrt(1.+24.*real(val)) + 1.)/6.)
end function is_pentagonal

pure logical function is_hexaonal(val)
  integer(int64), intent(in) :: val

  is_hexaonal = is_integer(.25*(sqrt(1.+8.*real(val)) + 1.))
end function is_hexaonal

pure logical function is_heptagonal(val)
  integer(int64), intent(in) :: val

  is_heptagonal = is_integer((sqrt(9.+40.*real(val)) + 3.)/10.)
end function is_heptagonal

pure logical function is_octagonal(val)
  integer(int64), intent(in) :: val

  is_octagonal = is_integer((sqrt(1.+3.*real(val)) + 1.)/3.)
end function is_octagonal

pure logical function is_integer(val)
  real, intent(in) :: val

  is_integer = abs(val - nint(val)) < eps
end function is_integer

end submodule submodule_euler0061

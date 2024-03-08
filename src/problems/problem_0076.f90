submodule(module_problem) submodule_euler0076
!> Keywords: Partition (number theory), Pentagonal number theorem
implicit none
contains

module subroutine euler0076(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), allocatable :: array(:), tmp(:)
  integer(int64) :: i, j, k, n

  !> Iteratively generate partion array
  k = 10 
  n = 90
  array = [integer(int64) ::]
  do j = 0, n, k
    tmp = [(partition(i, array), i=j + 1, j + k)]
    array = [array, tmp]
  end do
  write (answer, "(i0)") partition(100_int64, array) - 1
end subroutine euler0076

!> Partition of a number, based on existing partition array.
pure recursive function partition(n, array) result(ret)
  integer(int64), intent(in) :: n, array(:)
  integer(int64) :: ret
  integer(int64) :: kmax, sign, np
  integer(int64) :: k, a, t, b

  t = ubound(array, dim=1)
  b = lbound(array, dim=1)
  if (n <= t .and. n >= b) then
    ret = array(n)
    return
  end if

  select case (n)
  case (0:1)
    ret = 1
  case (2)
    ret = 2
  case default
    k = 1
    do while (n - g(k) >= 0)
      k = k + 1
    end do
    kmax = k - 1

    ret = 0
    do k = 1, kmax
      do sign = -1, 1, 2
        np = n - g(sign*k)
        if (np <= t .and. np >= b) then
          a = array(np)
        else
          a = partition(np, array)
        end if
        ret = ret + (-1)**(sign*k - 1)*a
      end do
    end do
  end select
end function partition

!> Pentagonal number
elemental function g(k) result(ret)
  integer(int64), intent(in) :: k
  integer(int64) :: ret

  ret = k*(3*k - 1)/2
end function g

end submodule submodule_euler0076

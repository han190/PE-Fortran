module module_quicksort

use, intrinsic :: iso_fortran_env, only: int64
use :: module_utility, only: swap
implicit none

public :: quicksort
private 
contains

pure recursive subroutine quicksort(arr, low, high)
  integer(int64), intent(inout) :: arr(:)
  integer(int64), intent(in) :: low, high
  integer(int64) :: pivot_loc

  if (low < high) then
    call partition(arr, low, high, pivot_loc)
    call quicksort(arr, low, pivot_loc - 1)
    call quicksort(arr, pivot_loc + 1, high)
  end if
end subroutine quicksort

pure subroutine partition(arr, low, high, pivot_loc)
  integer(int64), intent(inout) :: arr(:)
  integer(int64), intent(in) :: low, high
  integer(int64), intent(out) :: pivot_loc
  integer(int64) :: i, j, pivot

  pivot = arr(high)
  i = low - 1
  do j = low, high
    if (arr(j) < pivot) then
      i = i + 1
      call swap(arr(i), arr(j))
    end if
  end do
  call swap(arr(i + 1), arr(high))
  pivot_loc = i + 1
end subroutine partition

end module module_quicksort

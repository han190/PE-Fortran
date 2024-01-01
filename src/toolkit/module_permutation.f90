module module_permutation

use, intrinsic :: iso_fortran_env, only: int64
use :: module_utility, only: swap
implicit none

public :: permutation_type
public :: new_permutation
public :: set_permutation
public :: permute
public :: permutable
public :: index
private

!> Permutation type
type :: permutation_type
  integer(int64) :: n, k
  integer(int64), allocatable :: indices(:)
  integer(int64), allocatable :: pool(:), cycles(:)
end type permutation_type

!> Index
interface index
  module procedure :: current_permutation
end interface index

contains

!> Constructor
pure function new_permutation(n, k, start) result(permutation)
  integer(int64), intent(in) :: n
  integer(int64), intent(in), optional :: k, start(:)
  type(permutation_type) :: permutation
  integer(int64) :: i, rule

  permutation%n = n
  if (present(k)) then
    permutation%k = k
  else
    permutation%k = 0
  end if

  if (permutation%k > permutation%n .or. permutation%k < 0) then
    error stop "[new_permutation] Invalid k."
  else if (permutation%n < 0) then
    error stop "[new_permutation] Invalid n."
  end if

  if (.not. present(start)) then
    if (permutation%k /= 0) then
      permutation%indices = [(i, i=1, permutation%k)]
      permutation%pool = [(i, i=1, permutation%n)]
      permutation%cycles = [(permutation%n - i + 1, i=1, permutation%k)]
    else
      permutation%indices = [(i, i=1, permutation%n)]
    end if
  else
    call set_permutation(permutation, start)
  end if
end function new_permutation

!> For a given array, put selected elements to the front.
pure function reorder(array, elements) result(ret)
  integer(int64), intent(in) :: array(:), elements(:)
  integer(int64), allocatable :: ret(:)
  logical :: selected(size(array))
  integer(int64) :: i
  character(len=:), allocatable :: error_message

  error_message = "[reorder] size(elements) > size(array)"
  if (size(elements) > size(array)) error stop error_message

  selected = .true.
  error_message = "[reorder] Invalid element."
  do i = 1, size(elements)
    associate (e => elements(i))
      if (e > size(selected) .or. e <= 0) error stop error_message
      selected(e) = .false.
    end associate
  end do
  ret = [elements, pack(array, mask=selected)]
end function reorder

!> Set permutation with new indices
pure subroutine set_permutation(permutation, indices)
  type(permutation_type), intent(inout) :: permutation
  integer(int64), intent(in) :: indices(:)
  integer(int64), allocatable :: pool(:), ordered(:)
  logical, allocatable :: selected(:)
  integer(int64) :: i

  if (permutation%k /= 0) then
    if (.not. allocated(permutation%cycles)) then
      allocate (permutation%cycles(permutation%k))
    else if (size(permutation%cycles) /= permutation%k) then
      deallocate (permutation%cycles)
      allocate (permutation%cycles(permutation%k))
    end if

    permutation%indices = indices
    ordered = [(i, i=1, permutation%n)]
    permutation%pool = reorder(ordered, indices)
    permutation%cycles(1) = permutation%n - permutation%indices(1) + 1
    do i = 2, permutation%k
      pool = reorder(ordered, permutation%indices(1:i - 1))
      permutation%cycles(i) = permutation%n - &
        & findloc(pool, permutation%indices(i), dim=1) + 1
    end do
  else
    permutation%indices = indices
  end if
end subroutine set_permutation

!> Permute
pure subroutine permute(permutation)
  type(permutation_type), intent(inout) :: permutation
  integer(int64) :: i, j, tmp

  if (permutation%k /= 0) then
    do i = permutation%k, 1, -1
      permutation%cycles(i) = permutation%cycles(i) - 1
      if (permutation%cycles(i) == 0) then
        permutation%pool(i:) = [permutation%pool(i + 1:), &
          & permutation%pool(i)]
        permutation%cycles(i) = permutation%n - i + 1
      else
        j = permutation%cycles(i)
        call swap(permutation%pool(permutation%n - j + 1), &
          & permutation%pool(i))
        permutation%indices = permutation%pool(:permutation%k)
        exit
      end if
    end do
  else
    do i = permutation%n - 1, 1, -1
      if (permutation%indices(i) < permutation%indices(i + 1)) exit
    end do
    if (i == 0) return
    do j = permutation%n, 1, -1
      if (permutation%indices(i) < permutation%indices(j)) exit
    end do
    call swap(permutation%indices(j), permutation%indices(i))
    permutation%indices(i + 1:permutation%n) = &
      & permutation%indices(permutation%n:i + 1:-1)
  end if
end subroutine permute

!> Permutable
function permutable(permutation, check_only) result(ret)
  type(permutation_type), intent(inout) :: permutation
  logical :: ret
  logical, intent(in), optional :: check_only
  integer(int64) :: i

  if (permutation%k /= 0) then
    ret = any(permutation%cycles /= 1)
  else
    ret = .not. all(permutation%indices == &
      & [(i, i=permutation%n, 1, -1)])
  end if

  if (ret) then
    if (present(check_only)) then
      if (.not. check_only) call permute(permutation)
    else !> Default
      call permute(permutation)
    end if
  end if
end function permutable

!> Current permutation
pure function current_permutation(permutation) result(ret)
  type(permutation_type), intent(in) :: permutation
  integer(int64), allocatable :: ret(:)

  ret = permutation%indices
end function current_permutation

end module module_permutation

module module_permutation

use, intrinsic :: iso_fortran_env, only: int64
use, non_intrinsic :: module_utility, only: swap
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
  integer(int64), allocatable :: indices(:), pool(:), cycles(:)
  procedure(permute_procedure), pointer :: permute
contains
  final :: destructor
end type permutation_type

!> Index
interface index
  module procedure :: current_permutation
end interface index

!> Interface to permute
abstract interface
  pure subroutine permute_procedure(permutation)
    import :: permutation_type
    class(permutation_type), intent(inout) :: permutation
  end subroutine permute_procedure
end interface

contains

!> Constructor
pure function new_permutation(n, k, start) result(permutation)
  integer(int64), intent(in) :: n
  integer(int64), intent(in), optional :: k, start(:)
  type(permutation_type) :: permutation
  integer(int64) :: i

  permutation%n = n
  if (present(k)) then
    permutation%k = k
    permutation%permute => permutation_kn
  else
    permutation%k = 0
    permutation%permute => permutation_n
  end if

  if (permutation%k > permutation%n .or. permutation%k < 0) then
    error stop "[new_permutation] Invalid k."
  else if (permutation%n < 0) then
    error stop "[new_permutation] Invalid n."
  end if

  if (present(start)) then
    call set_permutation(permutation, start)
    return
  end if

  if (permutation%k /= 0) then
    permutation%indices = [(i, i=1, permutation%k)]
    permutation%pool = [(i, i=1, permutation%n)]
    permutation%cycles = [(permutation%n - i + 1, i=1, permutation%k)]
  else
    permutation%indices = [(i, i=1, permutation%n)]
  end if
end function new_permutation

!> For a given array, put selected elements to the front.
!> (Assuming the array is filled with integers 
!>  from 1 to the size of array.)
pure function reorder(array, elements) result(ret)
  integer(int64), intent(in) :: array(:), elements(:)
  integer(int64), allocatable :: ret(:)
  logical :: selected(size(array))

  selected = .true.
  selected(elements) = .false.
  ret = [elements, pack(array, mask=selected)]
end function reorder

!> Set permutation with new indices
pure subroutine set_permutation(permutation, indices)
  type(permutation_type), intent(inout) :: permutation
  integer(int64), intent(in) :: indices(:)
  integer(int64), allocatable :: pool(:), ordered(:)
  ! logical, allocatable :: selected(:)
  integer(int64) :: i, n

  if (permutation%k == 0) then
    permutation%indices = indices
    return
  end if

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
    n = findloc(pool, permutation%indices(i), dim=1)
    permutation%cycles(i) = permutation%n - n + 1
  end do
end subroutine set_permutation

!> "Toggle the gear" (used in k-permutation of n)
pure subroutine toggle(toggleable, permutation, i)
  logical, intent(out) :: toggleable
  type(permutation_type), intent(inout) :: permutation
  integer(int64), intent(in) :: i
  integer(int64) :: j

  toggleable = permutation%cycles(i) == 0
  if (toggleable) then
    permutation%pool(i:) = [permutation%pool(i + 1:), permutation%pool(i)]
    permutation%cycles(i) = permutation%n - i + 1
  else
    j = permutation%n - permutation%cycles(i) + 1
    call swap(permutation%pool(j), permutation%pool(i))
    permutation%indices = permutation%pool(:permutation%k)
  end if
end subroutine toggle

!> k-permutation of n
pure subroutine permutation_kn(permutation)
  class(permutation_type), intent(inout) :: permutation
  integer(int64) :: i
  logical :: toggleable

  do i = permutation%k, 1, -1
    permutation%cycles(i) = permutation%cycles(i) - 1
    call toggle(toggleable, permutation, i)
    if (.not. toggleable) exit
  end do
end subroutine permutation_kn

!> permutation of n
pure subroutine permutation_n(permutation)
  class(permutation_type), intent(inout) :: permutation
  integer(int64) :: i, j

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
end subroutine permutation_n

!> Permutable
function permutable(permutation, check_only) result(ret)
  type(permutation_type), intent(inout) :: permutation
  logical, intent(in), optional :: check_only
  logical :: ret
  integer(int64) :: i

  if (permutation%k == 0) then
    ret = any(permutation%indices /= [(i, i=permutation%n, 1, -1)])
  else
    ret = any(permutation%cycles /= 1)
  end if
  if (.not. ret) return

  if (present(check_only)) then
    if (.not. check_only) call permutation%permute()
  else !> Default
    call permutation%permute()
  end if
end function permutable

!> Current permutation
pure function current_permutation(permutation) result(ret)
  type(permutation_type), intent(in) :: permutation
  integer(int64), allocatable :: ret(:)

  ret = permutation%indices
end function current_permutation

!> Wrapper of the permute procedure
pure subroutine permute(permutation)
  type(permutation_type), intent(inout) :: permutation

  call permutation%permute()
end subroutine permute

!> Destructor
impure elemental subroutine destructor(permutation)
  type(permutation_type), intent(inout) :: permutation

  if (associated(permutation%permute)) nullify (permutation%permute)
end subroutine destructor

end module module_permutation

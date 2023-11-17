module module_permutation

use :: iso_fortran_env, only: int64
implicit none

public :: permutation_type
public :: initialize
public :: permutable
public :: permute
public :: write(formatted)
public :: index
private

!> Permutation type
type :: permutation_type(n, k)
  integer(int64), len :: n
  integer(int64), len :: k = 0_int64
  integer(int64), allocatable :: indices(:)
  integer(int64) :: pool(n)
  integer(int64), allocatable :: cycles(:)
end type permutation_type

!> DDTIO
interface write(formatted)
  module procedure :: write_formatted
end interface write(formatted)

!> Index
interface index
  module procedure :: current_permutation
end interface index

contains

!> Reallocation process
pure subroutine reallocate(arr, n)
  integer(int64), allocatable, intent(inout) :: arr(:)
  integer(int64), intent(in) :: n

  if (allocated(arr)) then
    if (size(arr) /= n) then
      deallocate (arr)
      allocate (arr(n))
    end if
  else
    allocate (arr(n))
  end if
end subroutine reallocate

!> Constructor
pure subroutine initialize(permutation, start)
  type(permutation_type(n=*, k=*)), intent(inout) :: permutation
  integer(int64), intent(in), optional :: start(:)
  integer(int64) :: i, k

  if (permutation%k > permutation%n .or. permutation%k < 0) then
    error stop "[initialize] Invalid k."
  else if (permutation%n < 0) then
    error stop "[initialize] Invalid n."
  end if

  k = merge(permutation%n, permutation%k, permutation%k == 0)
  call reallocate(permutation%indices, k)
  call reallocate(permutation%cycles, k)

  if (.not. present(start)) then
    do i = 1, permutation%n
      permutation%pool(i) = i
    end do
    do i = 1, k
      permutation%indices(i) = i
      permutation%cycles(i) = permutation%n - i + 1
    end do
  else
    call set_permutation(permutation, start)
  end if
end subroutine initialize

!> For a given array, put selected elements to the front.
pure function reorder(array, elements) result(ret)
  integer(int64), intent(in) :: array(:), elements(:)
  integer(int64), allocatable :: ret(:)
  logical :: selected(size(array))
  integer(int64) :: i
  character(len=:), allocatable :: error_message

  error_message = "size(elements) > size(array)"
  if (size(elements) > size(array)) error stop error_message

  selected = .true.
  error_message = "[reorder_int64] Invalid element."
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
  type(permutation_type(n=*, k=*)), intent(inout) :: permutation
  integer(int64), intent(in) :: indices(:)
  integer(int64), allocatable :: pool(:), ordered(:)
  logical, allocatable :: selected(:)
  integer(int64) :: i, k

  k = size(permutation%indices)
  permutation%indices = indices
  ordered = [(i, i=1, permutation%n)]
  permutation%pool = reorder(ordered, indices)
  permutation%cycles(1) = permutation%n - permutation%indices(1) + 1
  do i = 2, k
    pool = reorder(ordered, permutation%indices(1:i - 1))
    permutation%cycles(i) = permutation%n - &
      & findloc(pool, permutation%indices(i), dim=1) + 1
  end do
end subroutine set_permutation

!> Permutable
function permutable(permutation, check_only) result(ret)
  type(permutation_type(n=*, k=*)), intent(inout) :: permutation
  logical :: ret
  logical, intent(in), optional :: check_only

  if (present(check_only)) then
    if (.not. check_only) call permute(permutation)
  else !> Default
    call permute(permutation)
  end if
  ret = any(permutation%cycles /= 1)
end function permutable

!> Swap
pure subroutine swap(a, b)
  integer(int64), intent(inout) :: a, b
  integer(int64) :: tmp

  tmp = a
  a = b
  b = tmp
end subroutine swap

!> Permute
pure subroutine permute(permutation)
  type(permutation_type(n=*, k=*)), intent(inout) :: permutation
  integer(int64) :: i, j, k, tmp

  k = size(permutation%indices)
  do i = k, 1, -1
    permutation%cycles(i) = permutation%cycles(i) - 1
    if (permutation%cycles(i) == 0) then
      permutation%pool(i:) = [permutation%pool(i + 1:), permutation%pool(i)]
      permutation%cycles(i) = permutation%n - i + 1
    else
      j = permutation%cycles(i)
      call swap(permutation%pool(i), permutation%pool(permutation%n - j + 1))
      permutation%indices = permutation%pool(:k)
      exit
    end if
  end do
end subroutine permute

!> Current permutation
pure function current_permutation(permutation, option) result(ret)
  type(permutation_type(n=*, k=*)), intent(in) :: permutation
  character(len=*), intent(in), optional :: option
  integer(int64), allocatable :: ret(:)

  ret = permutation%indices
end function current_permutation

subroutine write_formatted(permutation, unit, iotype, v_list, iostat, iomsg)
  class(permutation_type(n=*, k=*)), intent(in) :: permutation
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype
  integer, intent(in)  :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg
  character(len=:), allocatable :: fmt, n

  if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
    select case (trim(iomsg))
    case ("Meta")
      if (permutation%k == 0) then
        fmt = "('Permutation of ', i0, ' (int64)')"
        write (unit, fmt, iostat=iostat) permutation%n
      else
        fmt = "(i0, '-Permutation of ', i0, ' (int64)')"
        write (unit, fmt, iostat=iostat) &
          & permutation%k, permutation%n
      end if
    case ("Cycles")
      allocate (character(len=100) :: n)
      write (n, "(i0)") size(permutation%indices) - 1
      fmt = "('[', "//trim(n)//"(i0, ',', 1x), i0, ']')"
      write (unit, fmt, iostat=iostat) permutation%cycles
    case default
      allocate (character(len=100) :: n)
      write (n, "(i0)") size(permutation%indices) - 1
      fmt = "('[', "//trim(n)//"(i0, ',', 1x), i0, ']')"
      write (unit, fmt, iostat=iostat) permutation%indices
    end select
  end if
end subroutine write_formatted

end module module_permutation

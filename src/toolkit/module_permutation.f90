module module_permutation

use :: module_constant
implicit none

public :: permutation_type
public :: initialize
public :: permutable
public :: permute
public :: write(formatted)
private

!> Permutation type
type :: permutation_type(n, k, kind)
  integer, len :: n
  integer, len :: k
  integer, kind :: kind = int32
  integer(kind=kind) :: indices(k)
  integer(kind=kind) :: pool(n)
  integer(kind=kind) :: cycles(k)
end type permutation_type

interface initialize
  module procedure :: initialize_int32
  module procedure :: initialize_int64
end interface initialize

interface permutable
  module procedure :: permutable_int32
  module procedure :: permutable_int64
end interface permutable

interface permute
  module procedure :: permute_int32
  module procedure :: permute_int64
end interface permute

interface write(formatted)
  module procedure :: write_formatted_int32
  module procedure :: write_formatted_int64
end interface write(formatted)

contains

pure subroutine initialize_int32(permutation, start)
  type(permutation_type(n=*, k=*, kind=int32)), intent(inout) :: permutation
  integer(int32), intent(in), optional :: start(:)
  integer(int32) :: i, tmp

  if (permutation%k > permutation%n) &
    & error stop "[initialize_int32] k > n!"
  if (.not. present(start)) then
    do i = 1, permutation%n
      permutation%pool(i) = i
    end do

    do i = 1, permutation%k
      permutation%indices(i) = i
      permutation%cycles(i) = permutation%n - i + 1
    end do
  else
    call set_permutation_int32(permutation, start)
  end if
end subroutine initialize_int32

pure subroutine initialize_int64(permutation, start)
  type(permutation_type(n=*, k=*, kind=int64)), intent(inout) :: permutation
  integer(int64), intent(in), optional :: start(:)
  integer(int64) :: i

  if (permutation%k > permutation%n) &
    & error stop "[initialize_int64] k > n!"
  if (.not. present(start)) then
    do i = 1, permutation%n
      permutation%pool(i) = i
    end do

    do i = 1, permutation%k
      permutation%indices(i) = i
      permutation%cycles(i) = permutation%n - i + 1
    end do
  else
    call set_permutation_int64(permutation, start)
  end if
end subroutine initialize_int64

!> For a given array, put selected elements to the front.
pure function reorder_int32(array, elements) result(ret)
  integer(int32), intent(in) :: array(:), elements(:)
  integer(int32), allocatable :: ret(:)
  logical :: selected(size(array))
  integer(int32) :: i
  character(len=:), allocatable :: error_message

  error_message = "size(elements) > size(array)"
  if (size(elements) > size(array)) error stop error_message

  selected = .true.
  error_message = "[reorder_int32] Invalid element."
  do i = 1, size(elements)
    associate (e => elements(i))
      if (e > size(selected) .or. e <= 0) error stop error_message
      selected(e) = .false.
    end associate
  end do
  ret = [elements, pack(array, mask=selected)]
end function reorder_int32

!> For a given array, put selected elements to the front.
pure function reorder_int64(array, elements) result(ret)
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
end function reorder_int64

!> Set permutation with new indices
pure subroutine set_permutation_int32(permutation, indices)
  type(permutation_type(n=*, k=*, kind=int32)), intent(inout) :: permutation
  integer(int32), intent(in) :: indices(:)
  integer(int32), allocatable :: pool(:), ordered(:)
  logical, allocatable :: selected(:)
  integer(int32) :: i

  if (size(indices) /= permutation%k) &
    & error stop "[set_permutation_int32] Invalid indices size."
  permutation%indices = indices
  ordered = [(i, i=1, permutation%n)]
  permutation%pool = reorder_int32(ordered, indices)
  if (size(permutation%cycles) /= permutation%k) &
    & error stop "[set_permutation_int32] Invalid cycles size."
  permutation%cycles(1) = permutation%n - permutation%indices(1) + 1
  do i = 2, permutation%k
    pool = reorder_int32(ordered, permutation%indices(1:i - 1))
    permutation%cycles(i) = permutation%n - &
      & findloc(pool, permutation%indices(i), dim=1) + 1
  end do
end subroutine set_permutation_int32

!> Set permutation with new indices
pure subroutine set_permutation_int64(permutation, indices)
  type(permutation_type(n=*, k=*, kind=int64)), intent(inout) :: permutation
  integer(int64), intent(in) :: indices(:)
  integer(int64), allocatable :: pool(:), ordered(:)
  logical, allocatable :: selected(:)
  integer(int64) :: i

  if (size(indices) /= permutation%k) &
    & error stop "[set_permutation_int64] Invalid indices size."
  permutation%indices = indices
  ordered = [(i, i=1, permutation%n)]
  permutation%pool = reorder_int64(ordered, indices)
  if (size(permutation%cycles) /= permutation%k) &
    & error stop "[set_permutation_int64] Invalid cycles size."
  permutation%cycles(1) = permutation%n - permutation%indices(1) + 1
  do i = 2, permutation%k
    pool = reorder_int64(ordered, permutation%indices(1:i - 1))
    permutation%cycles(i) = permutation%n - &
      & findloc(pool, permutation%indices(i), dim=1) + 1
  end do
end subroutine set_permutation_int64

!> Permutable
logical function permutable_int32(permutation, check_only)
  type(permutation_type(n=*, k=*, kind=int32)), intent(inout) :: permutation
  logical, intent(in), optional :: check_only

  if (present(check_only)) then
    if (check_only) then
      permutable_int32 = any(permutation%cycles /= 1)
    else
      call permute_int32(permutation)
      permutable_int32 = any(permutation%cycles /= 1)
    end if
  else !> Default
    call permute_int32(permutation)
    permutable_int32 = any(permutation%cycles /= 1)
  end if
end function permutable_int32

!> Permutable
logical function permutable_int64(permutation, check_only)
  type(permutation_type(n=*, k=*, kind=int64)), intent(inout) :: permutation
  logical, intent(in), optional :: check_only

  if (present(check_only)) then
    if (check_only) then
      permutable_int64 = any(permutation%cycles /= 1)
    else
      call permute_int64(permutation)
      permutable_int64 = any(permutation%cycles /= 1)
    end if
  else !> Default
    call permute_int64(permutation)
    permutable_int64 = any(permutation%cycles /= 1)
  end if
end function permutable_int64

!> Permute
pure subroutine permute_int32(permutation)
  type(permutation_type(n=*, k=*, kind=int32)), intent(inout) :: permutation
  integer(int32) :: i, j, tmp

  do i = permutation%k, 1, -1
    permutation%cycles(i) = permutation%cycles(i) - 1
    if (permutation%cycles(i) == 0) then
      permutation%pool(i:) = [permutation%pool(i + 1:), permutation%pool(i)]
      permutation%cycles(i) = permutation%n - i + 1
    else
      j = permutation%cycles(i)
      call swap_int32(permutation%pool(i), permutation%pool(permutation%n - j + 1))
      permutation%indices = permutation%pool(:permutation%k)
      exit
    end if
  end do
end subroutine permute_int32

!> Permute
pure subroutine permute_int64(permutation)
  type(permutation_type(n=*, k=*, kind=int64)), intent(inout) :: permutation
  integer(int64) :: i, j, tmp

  do i = permutation%k, 1, -1
    permutation%cycles(i) = permutation%cycles(i) - 1
    if (permutation%cycles(i) == 0) then
      permutation%pool(i:) = [permutation%pool(i + 1:), permutation%pool(i)]
      permutation%cycles(i) = permutation%n - i + 1
    else
      j = permutation%cycles(i)
      call swap_int64(permutation%pool(i), permutation%pool(permutation%n - j + 1))
      permutation%indices = permutation%pool(:permutation%k)
      exit
    end if
  end do
end subroutine permute_int64

pure subroutine swap_int32(a, b)
  integer(int32), intent(inout) :: a, b
  integer(int32) :: tmp

  tmp = a
  a = b
  b = tmp
end subroutine swap_int32

pure subroutine swap_int64(a, b)
  integer(int64), intent(inout) :: a, b
  integer(int64) :: tmp

  tmp = a
  a = b
  b = tmp
end subroutine swap_int64

subroutine write_formatted_int32(permutation, unit, iotype, v_list, iostat, iomsg)
  class(permutation_type(n=*, k=*, kind=int32)), intent(in) :: permutation
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype
  integer, intent(in)  :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg
  character(len=:), allocatable :: fmt, n

  if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
    select case (trim(iomsg))
    case ("Meta")
      fmt = "(i0, '-Permutation of ', i0, ' (int32)')"
      write (unit, fmt, iostat=iostat) &
        & permutation%k, permutation%n
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
end subroutine write_formatted_int32

subroutine write_formatted_int64(permutation, unit, iotype, v_list, iostat, iomsg)
  class(permutation_type(n=*, k=*, kind=int64)), intent(in) :: permutation
  integer, intent(in) :: unit
  character(*), intent(in) :: iotype
  integer, intent(in)  :: v_list(:)
  integer, intent(out) :: iostat
  character(*), intent(inout) :: iomsg
  character(len=:), allocatable :: fmt, n

  if (iotype == "DT" .or. iotype == "LISTDIRECTED") then
    select case (trim(iomsg))
    case ("Meta")
      fmt = "(i0, '-Permutation of ', i0, ' (int64)')"
      write (unit, fmt, iostat=iostat) &
        & permutation%k, permutation%n
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
end subroutine write_formatted_int64

end module module_permutation
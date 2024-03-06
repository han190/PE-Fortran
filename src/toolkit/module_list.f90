module module_list

use, intrinsic :: iso_fortran_env, only: int64
implicit none

public :: list_type
public :: append, pop
private

!> Integer node type
type :: node_type
  integer(int64) :: item
  type(node_type), pointer :: next => null()
  type(node_type), pointer :: prev => null()
end type node_type

!> Integer list type
type :: list_type
  integer(int64) :: len
  type(node_type), pointer :: head => null()
  type(node_type), pointer :: tail => null()
end type list_type

contains

!> Node constructor
pure function new_node(item) result(node)
  integer(int64), intent(in) :: item
  type(node_type) :: node

  node%item = item
end function new_node

!> Desotry node pointers
pure subroutine destroy_node(node)
  type(node_type), intent(inout) :: node

  nullify (node%next)
  nullify (node%prev)
end subroutine destroy_node

!> Append list
pure subroutine append(list, item)
  type(list_type), intent(inout) :: list
  integer(int64), intent(in) :: item

  if (associated(list%tail)) then
    allocate (list%tail%next, source=new_node(item))
    list%tail%next%prev => list%tail
    list%tail => list%tail%next
  else
    allocate (list%head, source=new_node(item))
    list%tail => list%head
  end if
  list%len = list%len + 1
end subroutine append

!> Pop list
pure subroutine pop(list, item)
  type(list_type), intent(inout) :: list
  integer(int64), intent(out), optional :: item
  type(node_type), pointer :: current_node

  if (list%len == 0) then
    if (present(item)) error stop &
      & "[pop] List length = 0."
    return
  end if

  if (present(item)) item = list%tail%item
  current_node => list%tail
  nullify (current_node%prev%next)
  list%tail => current_node%prev
  call destroy_node(current_node)
  deallocate (current_node)
  list%len = list%len - 1
end subroutine pop

end module module_list
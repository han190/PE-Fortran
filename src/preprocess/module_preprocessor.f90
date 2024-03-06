module module_preprocessor
implicit none

public :: declaration_type, subroutine_type
public :: subroutine_to_src, declaration_to_src, trim2
private

type :: declaration_type
  character(len=:), allocatable :: name
  character(len=:), allocatable :: type
  character(len=:), allocatable :: attrs(:)
end type declaration_type

type :: subroutine_type
  character(len=:), allocatable :: prefix
  character(len=:), allocatable :: name
  type(declaration_type), allocatable :: args(:)
end type subroutine_type

integer, parameter :: line_len = 1000
character, parameter :: space = char(32)

contains

pure function trim2(str) result(ret)
  character(len=*), intent(in) :: str
  character(len=:), allocatable :: ret

  ret = adjustl(trim(str))
end function trim2

pure function declaration_to_src(declare) result(ret)
  type(declaration_type), intent(in) :: declare
  character(len=:), allocatable :: ret
  integer :: i

  ret = trim2(declare%type)
  if (size(declare%attrs) > 0) then
    do i = 1, size(declare%attrs)
      ret = ret//","//space//trim2(declare%attrs(i))
    end do
  end if
  ret = ret//space//"::"//space//trim2(declare%name)
end function declaration_to_src

pure function subroutine_to_src(sub, indent) result(ret)
  type(subroutine_type), intent(in) :: sub
  integer, intent(in) :: indent
  character(len=:), allocatable :: ret(:)
  character(len=:), allocatable :: tmp
  integer :: i

  if (allocated(ret)) deallocate (ret)
  allocate (character(len=line_len) :: ret(size(sub%args) + 2))

  tmp = trim2(sub%prefix)//space//"subroutine"//space// &
    & trim2(sub%name)//"("//trim2(sub%args(1)%name)
  do i = 2, size(sub%args)
    tmp = tmp//","//space//trim2(sub%args(i)%name)
  end do
  ret(1) = tmp//")"

  do i = 2, size(ret) - 1
    ret(i) = repeat(space, indent)// &
      & declaration_to_src(sub%args(i - 1))
  end do
  ret(size(ret)) = "end subroutine"//space//trim2(sub%name)
end function subroutine_to_src

end module module_preprocessor
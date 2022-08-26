module long_integer_module

  use constant_module, only: i8, i32, i64
  implicit none

  public :: long_integer
  public :: assignment(=)
  public :: operator(==)
  public :: operator(>)
  public :: operator(<)
  public :: operator(>=)
  public :: operator(<=)

  public :: operator(+)
  public :: operator(-)

  private

  !> Long integer type.
  !> Assuming the number of digits
  !> is less than huge(0_i32).
  type :: long_integer
    character :: sign
    integer(i8), allocatable :: digit(:)
  end type long_integer

  !> Interface for assignment
  interface assignment(=)
    module procedure init_i32
    module procedure init_char
    module procedure init_i8_arr
  end interface assignment(=)

  !> Interface
  interface operator(==)
    module procedure eq
  end interface operator(==)

  interface operator(>)
    module procedure gt
  end interface operator(>)

  interface operator(<)
    module procedure lt
  end interface operator(<)

  interface operator(>=)
    module procedure ge
  end interface operator(>=)

  interface operator(<=)
    module procedure le
  end interface operator(<=)

  interface operator(+)
    module procedure add
  end interface operator(+)

  interface operator(-)
    module procedure sub
  end interface operator(-)

  !> Interfaces for submodule
  interface
    pure module subroutine init_i32(val, int_)
      type(long_integer), intent(inout) :: val
      integer(i32), intent(in) :: int_
    end subroutine init_i32

    pure module subroutine init_char(val, char_)
      type(long_integer), intent(inout) :: val
      character(len=*), intent(in) :: char_
    end subroutine init_char

    pure module subroutine init_i8_arr(val, digit_)
      type(long_integer), intent(inout) :: val
      integer(i8), intent(in) :: digit_(:)
    end subroutine init_i8_arr

    pure module logical function eq(val1, val2)
      type(long_integer), intent(in) :: val1
      type(long_integer), intent(in) :: val2
    end function eq

    pure module logical function gt(val1, val2)
      type(long_integer), intent(in) :: val1
      type(long_integer), intent(in) :: val2
    end function gt

    pure module logical function lt(val1, val2)
      type(long_integer), intent(in) :: val1
      type(long_integer), intent(in) :: val2
    end function lt

    pure module logical function ge(val1, val2)
      type(long_integer), intent(in) :: val1
      type(long_integer), intent(in) :: val2
    end function

    pure module logical function le(val1, val2)
      type(long_integer), intent(in) :: val1
      type(long_integer), intent(in) :: val2
    end function le

    pure module function add(val1, val2) result(ret)
      type(long_integer), intent(in) :: val1, val2
      type(long_integer) :: ret
    end function add

    pure module function sub(val2, val1) result(ret)
      type(long_integer), intent(in) :: val2
      type(long_integer), intent(in) :: val1
      type(long_integer) :: ret
    end function sub
  end interface

end module long_integer_module

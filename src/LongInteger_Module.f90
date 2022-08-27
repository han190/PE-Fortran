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

  public :: int

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
    module procedure init_str
    module procedure init_i32_arr
  end interface assignment(=)

  !> Interface
  interface int
    module procedure to_long_scal
    module procedure to_long_vect
  end interface int

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

    pure module subroutine init_str(val, char_)
      type(long_integer), intent(inout) :: val
      character(len=*), intent(in) :: char_
    end subroutine init_str

    pure module subroutine init_i32_arr(val, digs)
      type(long_integer), intent(inout) :: val
      integer(i32), intent(in) :: digs(:)
    end subroutine init_i32_arr

    elemental module function to_long_scal(val, kind) result(ret)
      class(*), intent(in) :: val
      character(*), intent(in) :: kind
      type(long_integer) :: ret
    end function to_long_scal

    pure module function to_long_vect(vals, kind) result(ret)
      class(*), intent(in) :: vals(:)
      character(*), intent(in) :: kind
      type(long_integer) :: ret
    end function to_long_vect

    pure module logical function eq(val1, val2)
      type(long_integer), intent(in) :: val1, val2
    end function eq

    pure module logical function gt(val1, val2)
      type(long_integer), intent(in) :: val1, val2
    end function gt

    pure module logical function lt(val1, val2)
      type(long_integer), intent(in) :: val1, val2
    end function lt

    pure module logical function ge(val1, val2)
      type(long_integer), intent(in) :: val1, val2
    end function

    pure module logical function le(val1, val2)
      type(long_integer), intent(in) :: val1, val2
    end function le

    pure module function add(val1, val2) result(ret)
      type(long_integer), intent(in) :: val1, val2
      type(long_integer) :: ret
    end function add

    pure module function sub(val2, val1) result(ret)
      type(long_integer), intent(in) :: val2, val1
      type(long_integer) :: ret
    end function sub
  end interface

end module long_integer_module

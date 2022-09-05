module constant_module

  use iso_fortran_env, only: int32, int64, real32, real64
  implicit none
  public

  integer, parameter :: sp = real32
  integer, parameter :: dp = real64
  integer, parameter :: i32 = int32
  integer, parameter :: i64 = int64

  real(sp), parameter :: tiny_sp = tiny(0._sp)
  real(dp), parameter :: tiny_dp = tiny(0._dp)

  character(:), allocatable :: data_path

  !> Array type
  type :: array_type
    integer(i32), allocatable :: array(:)
  end type array_type

end module constant_module
module constant_m

    use iso_fortran_env, only: int64, int32, real32, real64
    implicit none
    private

    integer, parameter, public :: sp = real32, dp = real64
    integer, parameter, public :: i32 = int32, i64 = int64
    real(sp), parameter, public :: tiny_sp = tiny(0._sp)
    real(dp), parameter, public :: tiny_dp = tiny(0._dp)

end module constant_m

module module_constant

use :: iso_fortran_env, only: int8, int32, int64, real32, real64
implicit none
public

!> Type constant
integer, parameter :: sp = real32
integer, parameter :: dp = real64

end module module_constant
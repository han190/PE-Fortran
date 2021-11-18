module euler_utils_m

    use euler_constant_m, only: sp, dp, i32, i64
    implicit none

    public :: operator(.UnitDigit.)
    public :: operator(.NumberOfDigits.)
    public :: operator(.ToArray.)
    public :: operator(.ToInteger.)

    !> Return unit digit of an integer.
    interface operator(.UnitDigit.)
        module procedure unit_digit_i32, unit_digit_i64
    end interface operator(.UnitDigit.)

    !> Return number of digits in an integer.
    interface operator(.NumberOfDigits.)
        module procedure number_of_digits_i32, number_of_digits_i64
    end interface operator(.NumberOfDigits.)

    !> Convert an intger to an array.
    interface operator(.ToArray.)
        module procedure i32_to_array, i64_to_array
    end interface operator(.ToArray.)

    !> Convert an array to an integer.
    interface operator(.ToInteger.)
        module procedure array_to_i32, array_to_i64
    end interface operator(.ToInteger.)

contains

    !> Return unit digit of a 32-bit integer.
    elemental integer(i32) function unit_digit_i32(n)
        integer(i32), intent(in) :: n

        unit_digit_i32 = mod(n, 10_i32)
    end function unit_digit_i32

    !> Return unit digit of a 64-bit integer.
    elemental integer(i64) function unit_digit_i64(n)
        integer(i64), intent(in) :: n

        unit_digit_i64 = mod(n, 10_i64)
    end function unit_digit_i64

    !> Return number of digits in a 32-bit integer.
    elemental integer(i32) function number_of_digits_i32(n)
        integer(i32), intent(in) :: n

        number_of_digits_i32 = floor(log10(real(n, sp))) + 1_i32
    end function number_of_digits_i32

    !> Return number of digits in a 64-bit integer.
    elemental integer(i64) function number_of_digits_i64(n)
        integer(i64), intent(in) :: n

        number_of_digits_i64 = floor(log10(real(n, sp))) + 1_i64
    end function number_of_digits_i64

    !> Convert a 32-bit integer into a 32-bit integer array.
    pure function i32_to_array(n) result(ret)
        integer(i32), intent(in) :: n
        integer(i32), allocatable :: ret(:)
        integer(i32) :: i

        if (n < 0) error stop ".ToArray.: n must be non-negative."
        ret = [(.UnitDigit. (n/10**(i - 1)), i=.NumberOfDigits.n, 1, -1)]
    end function i32_to_array

    !> Convert a 64-bit integer into a 64-bit integer array.
    pure function i64_to_array(n) result(ret)
        integer(i64), intent(in) :: n
        integer(i64), allocatable :: ret(:)
        integer(i64) :: i

        if (n < 0) error stop ".ToArray.: n must be non-negative."
        ret = [(.UnitDigit. (n/10**(i - 1)), i=.NumberOfDigits.n, 1, -1)]
    end function i64_to_array

    !> Convert an 32-bit integer array into an 32-bit integer.
    pure integer(i32) function array_to_i32(arr)
        integer(i32), intent(in) :: arr(:)
        integer(i32) :: i

        if (any(arr < 0)) error stop ".ToInteger.: arr must be non-negative."
        array_to_i32 = sum([(arr(i)*10**(size(arr) - i), i=1, size(arr))])
    end function array_to_i32

    !> Convert an 64-bit integer array into an 64-bit integer.
    pure integer(i64) function array_to_i64(arr)
        integer(i64), intent(in) :: arr(:)
        integer(i64) :: i

        if (any(arr < 0)) error stop ".ToInteger.: arr must be non-negative."
        array_to_i64 = sum([(arr(i)*10**(size(arr) - i), i=1, size(arr))])
    end function array_to_i64

end module euler_utils_m

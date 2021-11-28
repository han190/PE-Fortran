module utility_m

    use constant_m, only: sp, dp, i32, i64
    use multiprecision_m, only: multiprecision_t
    implicit none
    private

    public :: unit_digit, number_of_digits
    public :: to_array, to_integer
    public :: is_palindromic
    public :: number_of_divisors

    !> Return unit digit of an integer.
    interface unit_digit
        module procedure unit_digit_i32, unit_digit_i64
        module procedure unit_digit_multiprecision_t
    end interface unit_digit

    !> Return number of digits in an integer.
    interface number_of_digits
        module procedure number_of_digits_i32, number_of_digits_i64
        module procedure number_of_digits_multiprecision_t
    end interface number_of_digits

    !> Convert an intger to an array.
    interface to_array
        module procedure i32_to_array, i64_to_array
    end interface to_array

    !> Convert an array to an integer.
    interface to_integer
        module procedure array_to_i32, array_to_i64
    end interface to_integer

    !> To tell if an integer is palindromic.
    interface is_palindromic
        module procedure is_palindromic_i32
        module procedure is_palindromic_i64
    end interface is_palindromic

    !> Prime factorization.
    interface prime_factorization
        module procedure prime_factorization_i32
    end interface prime_factorization

    !> Number of divisors
    interface number_of_divisors
        module procedure number_of_divisors_i32
    end interface number_of_divisors

    !> Variant length array
    type, public :: variant_array_t
        integer(i32), allocatable :: array(:)
    end type variant_array_t

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

    !> Return unit digit of a multiprecision_t integer
    elemental integer(i32) function unit_digit_multiprecision_t(n)
        type(multiprecision_t), intent(in) :: n

        unit_digit_multiprecision_t = n%arr(size(n%arr))
    end function unit_digit_multiprecision_t

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

    !> Return number of digits in a multiprecsion_t integer.
    elemental integer(i32) function number_of_digits_multiprecision_t(n)
        type(multiprecision_t), intent(in) :: n

        number_of_digits_multiprecision_t = size(n%arr)
    end function number_of_digits_multiprecision_t

    !> Convert a 32-bit integer into a 32-bit integer array.
    pure function i32_to_array(n) result(ret)
        integer(i32), intent(in) :: n
        integer(i32), allocatable :: ret(:)
        integer(i32) :: i

        if (n < 0) error stop "to_array: n must be non-negative."
        ret = [(unit_digit(n/10**(i - 1)), i=number_of_digits(n), 1, -1)]
    end function i32_to_array

    !> Convert a 64-bit integer into a 64-bit integer array.
    pure function i64_to_array(n) result(ret)
        integer(i64), intent(in) :: n
        integer(i64), allocatable :: ret(:)
        integer(i64) :: i

        if (n < 0) error stop "to_array: n must be non-negative."
        ret = [(unit_digit(n/10**(i - 1)), i=number_of_digits(n), 1, -1)]
    end function i64_to_array

    !> Convert an 32-bit integer array into an 32-bit integer.
    pure integer(i32) function array_to_i32(arr)
        integer(i32), intent(in) :: arr(:)
        integer(i32) :: i

        if (any(arr < 0)) error stop "to_integer: arr must be non-negative."
        array_to_i32 = sum([(arr(i)*10**(size(arr) - i), i=1, size(arr))])
    end function array_to_i32

    !> Convert an 64-bit integer array into an 64-bit integer.
    pure integer(i64) function array_to_i64(arr)
        integer(i64), intent(in) :: arr(:)
        integer(i64) :: i

        if (any(arr < 0)) error stop "to_integer: arr must be non-negative."
        array_to_i64 = sum([(arr(i)*10**(size(arr) - i), i=1, size(arr))])
    end function array_to_i64

    !> To tell if an 32-bit integer is palindromic.
    pure logical function is_palindromic_i32(n)
        integer(i32), intent(in) :: n
        integer(i32) :: reversed, temp

        reversed = 0
        temp = n
        do while (temp > 0)
            reversed = reversed*10 + mod(temp, 10)
            temp = temp/10
        end do

        is_palindromic_i32 = .false.
        if (n == reversed) is_palindromic_i32 = .true.
    end function is_palindromic_i32

    !> To tell if an 64-bit integer is palindromic.
    pure logical function is_palindromic_i64(n)
        integer(i64), intent(in) :: n
        integer(i64) :: reversed, temp

        reversed = 0
        temp = n
        do while (temp > 0)
            reversed = reversed*10 + mod(temp, 10)
            temp = temp/10
        end do

        is_palindromic_i64 = .false.
        if (n == reversed) is_palindromic_i64 = .true.
    end function is_palindromic_i64

    !> Assuming an array of primes is already generated.
    pure subroutine prime_factorization_i32(n, primes, powers)
        integer(i32), intent(in) :: n, primes(:)
        integer(i32), intent(out) :: powers(size(primes))
        integer(i32) :: i, temp

        temp = n; powers = 0
        outer: do i = 1, size(primes)
            inner: do
                if (mod(temp, primes(i)) /= 0) then
                    exit inner
                else if (temp == 0) then
                    exit outer
                end if

                powers(i) = powers(i) + 1
                temp = temp/primes(i)
            end do inner
        end do outer
    end subroutine prime_factorization_i32

    !> Number of proper divisors.
    !> Write an integer in a form of n = p1**e1 + p1**e2 + ...
    !> where p_i are the primes and e_i are the number of existences.
    !> Then, number of divisors is (e1 + 1)*(e2 + 1)*...
    pure integer(i32) function number_of_divisors_i32(n, primes)
        integer(i32), intent(in) :: n, primes(:)
        integer(i32) :: powers(size(primes))

        call prime_factorization(n, primes, powers)
        number_of_divisors_i32 = product(powers + 1)
    end function number_of_divisors_i32

end module utility_m

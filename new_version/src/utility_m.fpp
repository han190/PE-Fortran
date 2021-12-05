module utility_m

    use constant_m, only: sp, i32, i64
    use multiprecision_m, only: multiprecision_t
    implicit none
    private

    public :: unit_digit, number_of_digits
    public :: to_array, to_integer
    public :: is_palindromic
    public :: prime_factorization
    public :: number_of_divisors

    #:set integer_kinds = ['i32', 'i64']

    !> Return unit digit of an integer.
    interface unit_digit
        #: for T in integer_kinds
        module procedure unit_digit_${T}$
        #: endfor
        module procedure unit_digit_multiprecision_t
    end interface unit_digit

    !> Return number of digits in an integer.
    interface number_of_digits
        #: for T in integer_kinds
        module procedure number_of_digits_${T}$
        #: endfor
        module procedure number_of_digits_multiprecision_t
    end interface number_of_digits

    !> Convert an intger to an array.
    interface to_array
        #: for T in integer_kinds
        module procedure to_array_${T}$
        #: endfor
    end interface to_array

    !> Convert an array to an integer.
    interface to_integer
        #: for T in integer_kinds
        module procedure to_integer_${T}$
        #: endfor
    end interface to_integer

    !> To tell if an integer is palindromic.
    interface is_palindromic
        #: for T in integer_kinds
        module procedure is_palindromic_${T}$
        #: endfor
    end interface is_palindromic

    !> Prime factorization.
    interface prime_factorization
        #: for T in integer_kinds
        module procedure prime_factorization_${T}$
        #: endfor
    end interface prime_factorization

    !> Number of divisors
    interface number_of_divisors
        #: for T in integer_kinds
        module procedure number_of_divisors_${T}$
        #: endfor
    end interface number_of_divisors

    !> Variant length array
    type, public :: variant_array_t
        integer(i32), allocatable :: array(:)
    end type variant_array_t

contains

    !> Return unit digit of an integer.
    #: for T in integer_kinds
    elemental integer(${T}$) function unit_digit_${T}$ (n)
        integer(${T}$), intent(in) :: n

        unit_digit_${T}$ = mod(n, 10_${T}$)
    end function unit_digit_${T}$
    #: endfor

    !> Return unit digit of a multiprecision_t integer
    elemental integer(i32) function unit_digit_multiprecision_t(n)
        type(multiprecision_t), intent(in) :: n

        unit_digit_multiprecision_t = n%arr(size(n%arr))
    end function unit_digit_multiprecision_t

    !> Return number of digits in an integer.
    #: for T in integer_kinds
    elemental integer(${T}$) function number_of_digits_${T}$ (n)
        integer(${T}$), intent(in) :: n

        number_of_digits_${T}$ = floor(log10(real(n, sp))) + 1_${T}$
    end function number_of_digits_${T}$
    #: endfor

    !> Return number of digits in a multiprecsion_t integer.
    elemental integer(i32) function number_of_digits_multiprecision_t(n)
        type(multiprecision_t), intent(in) :: n

        number_of_digits_multiprecision_t = size(n%arr)
    end function number_of_digits_multiprecision_t

    !> Convert an integer into an integer array.
    #: for T in integer_kinds
    pure function to_array_${T}$ (n) result(ret)
        integer(${T}$), intent(in) :: n
        integer(${T}$), allocatable :: ret(:)
        integer(${T}$) :: i

        if (n < 0) error stop "to_array: n must be non-negative."
        ret = [(unit_digit(n/10**(i - 1)), i=number_of_digits(n), 1, -1)]
    end function to_array_${T}$
    #: endfor

    !> Convert an integer array into an integer.
    #: for T in integer_kinds
    pure integer(${T}$) function to_integer_${T}$ (arr)
        integer(${T}$), intent(in) :: arr(:)
        integer(${T}$) :: i

        if (any(arr < 0)) error stop "to_integer: arr must be non-negative."
        to_integer_${T}$ = sum([(arr(i)*10**(size(arr) - i), i=1, size(arr))])
    end function to_integer_${T}$
    #: endfor

    !> To tell if an integer is palindromic.
    #: for T in integer_kinds
    pure logical function is_palindromic_${T}$ (n)
        integer(${T}$), intent(in) :: n
        integer(${T}$) :: reversed, temp

        reversed = 0_${T}$
        temp = n
        do while (temp > 0_${T}$)
            reversed = reversed*10_${T}$+mod(temp, 10_${T}$)
            temp = temp/10_${T}$
        end do

        is_palindromic_${T}$ = .false.
        if (n == reversed) is_palindromic_${T}$ = .true.
    end function is_palindromic_${T}$
    #: endfor

    !> Assuming an array of primes is already generated.
    #: for T in integer_kinds
    pure subroutine prime_factorization_${T}$ (n, primes, powers)
        integer(${T}$), intent(in) :: n, primes(:)
        integer(${T}$), intent(out) :: powers(size(primes))
        integer(${T}$) :: i, temp

        temp = n; powers = 0_${T}$
        outer: do i = 1_${T}$, size(primes)
            inner: do
                if (mod(temp, primes(i)) /= 0_${T}$) then
                    exit inner
                else if (temp == 0_${T}$) then
                    exit outer
                end if

                powers(i) = powers(i) + 1_${T}$
                temp = temp/primes(i)
            end do inner
        end do outer
    end subroutine prime_factorization_${T}$
    #: endfor

    !> Number of proper divisors.
    !> Write an integer in a form of n = p1**e1 + p2**e2 + ...,
    !> where p_i are the primes and e_i are the number of existences.
    !> Then, number of divisors is (e1 + 1)*(e2 + 1)*...
    #: for T in integer_kinds
    pure integer(${T}$) function number_of_divisors_${T}$ (n, primes)
        integer(${T}$), intent(in) :: n, primes(:)
        integer(${T}$) :: powers(size(primes))

        call prime_factorization(n, primes, powers)
        number_of_divisors_${T}$ = product(powers + 1)
    end function number_of_divisors_${T}$
    #: endfor

end module utility_m

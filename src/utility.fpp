#:set integer_kinds = ['i32', 'i64']

module utility_m

    use constant_m, only: sp, i32, i64
    implicit none
    private

    public :: unit_digit, number_of_digits, sqrt
    public :: to_array, to_integer
    public :: is_palindromic, is_pandigital
    public :: prime_factorization, number_of_divisors
    public :: lcm, gcd
    public :: swap, permute

    !> Return unit digit of an integer.
    interface unit_digit
        #: for T in integer_kinds
        module procedure unit_digit_${T}$
        #: endfor
    end interface unit_digit

    !> Return number of digits in an integer.
    interface number_of_digits
        #: for T in integer_kinds
        module procedure number_of_digits_${T}$
        #: endfor
    end interface number_of_digits

    !> Square root for integers.
    interface sqrt
        #: for T in integer_kinds
        module procedure sqrt_${T}$
        #: endfor
    end interface sqrt

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

    !> Number of divisors.
    interface number_of_divisors
        #: for T in integer_kinds
        module procedure number_of_divisors_${T}$
        #: endfor
    end interface number_of_divisors

    !> Is pandigital.
    interface is_pandigital
        #: for T in integer_kinds
        module procedure is_pandigital_${T}$
        #: endfor
    end interface is_pandigital

    !> Least common multiple.
    interface lcm
        #: for T in integer_kinds
        module procedure lcm_${T}$
        #: endfor
    end interface lcm

    !> Greatest common divisor.
    interface gcd
        #: for T in integer_kinds
        module procedure gcd_${T}$
        #: endfor
    end interface gcd

    !> Swap.
    interface swap
        #: for T in integer_kinds
        module procedure swap_${T}$
        #: endfor
        module procedure swap_char
    end interface swap

    !> Next permutation.
    interface permute
        #: for T in integer_kinds
        module procedure permute_${T}$
        module procedure permute_kn_${T}$
        #: endfor
    end interface permute

    !> Variant length array
    type, public :: variant_array_t
        integer(i32), allocatable :: array(:)
    end type variant_array_t

contains

    #: for T in integer_kinds
    !> Unit digit of an ${T}$ integer.
    elemental integer(${T}$) function unit_digit_${T}$ (n)
        integer(${T}$), intent(in) :: n

        unit_digit_${T}$ = mod(n, 10_${T}$)
    end function unit_digit_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Number of digits in an ${T}$ integer.
    elemental integer(${T}$) function number_of_digits_${T}$ (n)
        integer(${T}$), intent(in) :: n

        number_of_digits_${T}$ = floor(log10(real(n, sp))) + 1_${T}$
    end function number_of_digits_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Next permutation (k, n) for ${T}$.
    elemental integer(${T}$) function sqrt_${T}$ (n)
        integer(${T}$), intent(in) :: n

        sqrt_${T}$ = floor(sqrt(real(n, sp)), ${T}$)
    end function sqrt_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Convert an integer into an ${T}$ array.
    pure function to_array_${T}$ (n) result(ret)
        integer(${T}$), intent(in) :: n
        integer(${T}$), allocatable :: ret(:)
        integer(${T}$) :: i, temp

        temp = n
        associate (l => (number_of_digits(temp)))
            allocate (ret(l))
            do i = l, 1, -1
                ret(i) = unit_digit(temp)
                temp = temp/10_${T}$
            end do
        end associate
    end function to_array_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Convert an ${T}$ integer array into an ${T}$ integer.
    pure integer(${T}$) function to_integer_${T}$ (arr)
        integer(${T}$), intent(in) :: arr(:)
        integer(${T}$) :: i, temp

        temp = 0_${T}$
        do i = 1, size(arr)
            temp = temp*10_${T}$+arr(i)
        end do
        to_integer_${T}$ = temp
    end function to_integer_${T}$

    #: endfor

    #: for T in integer_kinds
    !> To tell if an ${T}$ integer is palindromic.
    elemental logical function is_palindromic_${T}$ (n)
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

    #: for T in integer_kinds
    !> Prime factorization of an ${T}$ integer.
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

    #: for T in integer_kinds
    !> Number of proper divisors of an ${T}$ integer.
    pure integer(${T}$) function number_of_divisors_${T}$ (n, primes)
        integer(${T}$), intent(in) :: n, primes(:)
        integer(${T}$) :: powers(size(primes))

        call prime_factorization(n, primes, powers)
        number_of_divisors_${T}$ = product(powers + 1)
    end function number_of_divisors_${T}$

    #: endfor

    #: for T in integer_kinds
    !> To tell if an ${T}$ integer is pandigital.
    pure logical function is_pandigital_${T}$ (n)
        integer(${T}$), intent(in) :: n
        integer(${T}$) :: temp, l
        logical, allocatable :: array(:)

        l = number_of_digits(n)
        if (l > 9_${T}$) l = 9_${T}$
        allocate (array(l))

        is_pandigital_${T}$ = .false.
        array = .false.
        temp = n

        do
            associate (u => (unit_digit(temp)))
                if (u == 0_${T}$ .or. u > l) exit
                array(u) = .true.
            end associate
            temp = temp/10
        end do

        if (count(array) == l) is_pandigital_${T}$ = .true.
    end function is_pandigital_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Greatest common divisor of two ${T}$ integers.
    pure recursive function gcd_${T}$ (a, b) result(ret)
        integer(${T}$), intent(in) :: a, b
        integer(${T}$) :: ret

        select case (b)
        case (0_${T}$)
            ret = a
        case default
            ret = gcd_${T}$ (b, mod(a, b))
        end select
    end function gcd_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Least common multiple of two ${T}$ integers.
    pure integer(${T}$) function lcm_${T}$ (a, b)
        integer(${T}$), intent(in) :: a, b

        lcm_${T}$ = abs(a*b)/gcd_${T}$ (a, b)
    end function lcm_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Swap for ${T}$.
    pure subroutine swap_${T}$ (a, b)
        integer(${T}$), intent(inout) :: a, b
        integer(${T}$) :: temp

        temp = a; a = b; b = temp
    end subroutine swap_${T}$

    #: endfor

    pure subroutine swap_char(a, b)
        character(len=*), intent(inout) :: a
        character(len=len(a)), intent(inout) :: b
        character(len=len(a)) :: temp

        temp = a; a = b; b = temp
    end subroutine swap_char

    #: for T in integer_kinds
    !> Next permutation of an ${T}$ array.
    pure subroutine permute_${T}$ (idx, next_avail)
        integer(${T}$), intent(inout) :: idx(:)
        logical, intent(out) :: next_avail
        integer(${T}$) :: i, j, temp

        next_avail = .false.
        do i = size(idx) - 1, 1, -1
            if (idx(i) < idx(i + 1)) then
                j = i
                next_avail = .true.
                exit
            end if
        end do
        if (.not. next_avail) return

        do i = size(idx), 1, -1
            if (idx(j) < idx(i)) exit
        end do

        call swap_${T}$ (idx(i), idx(j))
        idx(j + 1:size(idx)) = idx(size(idx):j + 1:-1)
    end subroutine permute_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Next permutation (k, n) of an ${T}$ array.
    pure subroutine permute_kn_${T}$ (k, n, idx, next_avail)
        integer(${T}$), intent(in) :: k, n
        integer(${T}$), intent(inout) :: idx(k)
        logical, intent(out) :: next_avail
        logical :: carried(k)
        integer(${T}$) :: i, j, temp

        next_avail = .true.
        associate (end => ([(i, i=n - k + 1, n)]))
            if (all(idx == end)) then
                next_avail = .false.
                return
            end if
        end associate

        carried = .true.
        label_carried_array: do i = k, 1, -1
            if (idx(i) == n - k + i) carried(i) = .false.
        end do label_carried_array

        if (all(carried)) then
            idx(k) = idx(k) + 1
        else
            associate (x => (findloc(carried, value=.false., dim=1) - 1))
                idx(x:k) = [(idx(x) + i, i=1, k - x + 1)]
            end associate
        end if
    end subroutine permute_kn_${T}$

    #: endfor

end module utility_m

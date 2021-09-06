module euler_utils_m
    use iso_fortran_env, only: int64, real32, real64
    implicit none
    private

    integer, parameter, public :: sp = real32, dp = real64
    real(sp), parameter, public :: tiny_sp = tiny(0._sp)
    real(dp), parameter, public :: tiny_dp = tiny(0._dp)

    public :: unit_digit, swap, digs_of_int, is_pandigital
    public :: fibonacci, reverse, is_palindromic, gcd, lcm, factorial
    public :: int_2_arr, arr_2_int, append, next_permutation

    !> A generic interface that returns the unit digit of an integer.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    print *, unit_digit(324) ! 4
    !>    print *, unit_digit(543212345_int64) ! 5
    !>end program main
    !>```
    interface unit_digit
        module procedure unit_digit_int32
        module procedure unit_digit_int64
    end interface unit_digit

    !> A generic interface that swap two elements (the two elements
    !> have to be the same type. When swapping two character types,
    !> the two character variables must have the same length.).
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    integer :: a, b
    !>    a = 32; b = 23
    !>    print *, a, b ! 32, 23
    !>    call swap(a, b)
    !>    print *, a, b ! 23, 32
    !>end program main
    !>```
    interface swap
        module procedure swap_sp, swap_dp
        module procedure swap_int32, swap_int64
        module procedure swap_equal_len_char
    end interface swap

    !> A generic interface that returns the length of an integer.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    user euler_utils_m
    !>    implicit none
    !>
    !>    print *, digs_of_int(12345) ! 5
    !>    print *, digs_of_int(1234567890_int64) ! 10
    !>end program main
    !>```
    interface digs_of_int
        module procedure digs_of_int_int32
        module procedure digs_of_int_int64
    end interface digs_of_int

    !> A generic interface that returns the n<sup>th</sup> fibonacci number.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    print *, fibonacci(12) ! 144
    !>    print *, fibonacci(12_int64) ! 144
    !>end program main
    !>```
    interface fibonacci
        module procedure fib32, fib64
    end interface fibonacci

    !> A generic interface that reverse the digits of an integer.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    print *, reverse(12345) ! 54321
    !>    print *, reverse(12345_int64) ! 54321
    !>end program main
    !>```
    interface reverse
        module procedure reverse_int32, reverse_int64
    end interface reverse

    !> A generic interface that tells if an integer is a palindromic integer.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    integer :: a = 123454321, b = 1234
    !>    print *, is_palindromic(a) ! T
    !>    print *, is_palindromic(b) ! F
    !>end program main
    !>```
    interface is_palindromic
        module procedure is_palindromic_int32
        module procedure is_palindromic_int64
    end interface is_palindromic

    !> Greatest common divisor.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    print *, gcd(32, 24) ! 8
    !>    print *, gcd(32_int64, 24_int64) ! 8
    !>end program main
    !>```
    interface gcd
        module procedure gcd_int32, gcd_int64
    end interface gcd

    !> Least common multiple.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    print *, lcm(3, 4) ! 12
    !>    print *, lcm(3_int64, 4_int64) ! 12
    !>end program main
    !>```
    interface lcm
        module procedure lcm_int32, lcm_int64
    end interface lcm

    !> Factorial.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    print *, factorial(4) ! 24
    !>    print *, factorial(4_int64) ! 24
    !>end program main
    !>```
    interface factorial
        module procedure factorial_int32
        module procedure factorial_int64
    end interface factorial

    !> To judge whether an integer is a pandigital number.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    print *, is_pandigital(1023456789) ! T
    !>end program main
    !>```
    interface is_pandigital
        module procedure is_pandigital_int32
        module procedure is_pandigital_int64
    end interface is_pandigital

    !> Convert integer to an integer array.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    integer :: a = 234
    !>    integer, allocatable :: arr(:)
    !>
    !>    call int_2_arr(a, arr)
    !>    print *, arr ! [2, 3, 4]
    !>end program main
    !>```
    interface int_2_arr
        module procedure int_2_arr_int32
        module procedure int_2_arr_int64
    end interface int_2_arr

    !> Convert an integer arr to an integer.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    integer :: arr(3) = [2, 3, 4]
    !>    integer :: a
    !>
    !>    call arr_2_int(arr, a)
    !>    print *, a ! 234
    !>end program main
    !>```
    interface arr_2_int
        module procedure arr_2_int_int32
        module procedure arr_2_int_int64
    end interface arr_2_int

    !> Append an element to the end of an array
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    integer, allocatable :: arr(:)
    !>
    !>    arr = [1, 2, 3]
    !>    call append(arr, 4)
    !>    print *, arr ! [1, 2, 3, 4]
    !>end program main
    !>```
    interface append
        module procedure append_sp, append_dp
        module procedure append_int32, append_int64
    end interface append

    !> An interface of variant permutation functions
    !>
    !>### Usage
    !>#### `next_permutation_int32/int64` (k-permutation of n)
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    integer :: k, n
    !>    integer, dimesion(2) :: arr
    !>    logical :: next_permutation_available
    !>
    !>    k = 2; n = 3
    !>    arr = [1, 2]
    !>    next_permutation_available = .true.
    !>
    !>    do while (next_permutation_available)
    !>        print *, arr
    !>        call permutation(k, n, arr, next_permutation_available)
    !>    end do
    !>    ! Output: (1, 2), (1, 3), and (2, 3).
    !>end program main
    !>```
    !>
    !>#### `next_permutation2_int32` (permutation)
    !>```fortran
    !>program main
    !>    use euler_utils_m
    !>    implicit none
    !>
    !>    integer :: arr(3)
    !>    logical :: next_permutation_available
    !>
    !>    arr = [1, 2, 3]
    !>    next_permutation_available = .true.
    !>
    !>    do while (next_permutation_available)
    !>        print *, arr
    !>        call permutation(arr, next_permutation_available)
    !>    end do
    !>    ! Output: (1, 2, 3), (1, 3, 2), (2, 1, 3), (2, 3, 1),
    !>    ! (3, 1, 2), and (3, 2, 1).
    !>end program main
    !>```
    interface next_permutation
        module procedure next_permutation_int32
        module procedure next_permutation_int64
        module procedure next_permutation2_int32
    end interface next_permutation

contains

    pure integer function unit_digit_int32(n)
        integer, intent(in) :: n
        unit_digit_int32 = mod(n, 10)
    end function unit_digit_int32

    pure integer(int64) function unit_digit_int64(n)
        integer(int64), intent(in) :: n
        unit_digit_int64 = mod(n, 10_int64)
    end function unit_digit_int64

    pure subroutine swap_sp(a, b)
        real(sp), intent(inout) :: a, b
        real(sp) :: tmp

        tmp = a; a = b; b = tmp
    end subroutine swap_sp

    pure subroutine swap_dp(a, b)
        real(dp), intent(inout) :: a, b
        real(dp) :: tmp

        tmp = a; a = b; b = tmp
    end subroutine swap_dp

    pure subroutine swap_int32(a, b)
        integer, intent(inout) :: a, b
        integer :: tmp

        tmp = a; a = b; b = tmp
    end subroutine swap_int32

    pure subroutine swap_int64(a, b)
        integer(int64), intent(inout) :: a, b
        integer(int64) :: tmp

        tmp = a; a = b; b = tmp
    end subroutine swap_int64

    pure subroutine swap_equal_len_char(a, b)
        character(len=*), intent(inout) :: a
        character(len=len(a)), intent(inout) :: b
        character(len=len(a)) :: tmp

        tmp = a
        a = b
        b = tmp
    end subroutine swap_equal_len_char

    pure integer function digs_of_int_int32(n)
        integer, intent(in) :: n

        digs_of_int_int32 = floor(log10(real(n, sp))) + 1
    end function digs_of_int_int32

    pure integer(int64) function digs_of_int_int64(n)
        integer(int64), intent(in) :: n

        digs_of_int_int64 = floor(log10(real(n, sp))) + 1_int64
    end function digs_of_int_int64

    pure recursive function fib32(n) result(ans)
        integer, intent(in) :: n
        integer :: k, ans

        if (n == 0) then
            ans = 0
        else if (n == 1 .or. n == 2) then
            ans = 1
        else if (mod(n, 2) == 0) then
            k = n/2
            ans = fib32(k)*(fib32(k + 1)*2 - fib32(k))
        else
            k = (n - 1)/2
            ans = fib32(k + 1)**2 + fib32(k)**2
        end if
    end function fib32

    pure recursive function fib64(n) result(ans)
        integer(int64), intent(in) :: n
        integer(int64) :: k, ans

        if (n == 0_int64) then
            ans = 0_int64
        else if (n == 1_int64 .or. n == 2_int64) then
            ans = 1_int64
        else if (mod(n, 2_int64) == 0_int64) then
            k = n/2_int64
            ans = fib64(k)*(fib64(k + 1_int64)*2_int64 - fib64(k))
        else
            k = (n - 1_int64)/2_int64
            ans = fib64(k + 1_int64)**2_int64 + fib64(k)**2_int64
        end if
    end function fib64

    pure integer function reverse_int32(n)
        integer, intent(in) :: n
        integer :: reversed, tmp

        reversed = 0; tmp = n
        do while (tmp > 0)
            reversed = reversed*10 + mod(tmp, 10)
            tmp = tmp/10
        end do
        reverse_int32 = reversed
    end function reverse_int32

    pure integer(int64) function reverse_int64(n)
        integer(int64), intent(in) :: n
        integer(int64) :: reversed, tmp
        reversed = 0_int64; tmp = n
        do while (tmp > 0_int64)
            reversed = reversed*10_int64 + mod(tmp, 10_int64)
            tmp = tmp/10_int64
        end do
        reverse_int64 = reversed
    end function reverse_int64

    pure logical function is_palindromic_int32(n)
        integer, intent(in) :: n

        is_palindromic_int32 = .false.
        if (n == reverse_int32(n)) then
            is_palindromic_int32 = .true.
        end if
    end function is_palindromic_int32

    pure logical function is_palindromic_int64(n)
        integer(int64), intent(in) :: n

        is_palindromic_int64 = .false.
        if (n == reverse_int64(n)) then
            is_palindromic_int64 = .true.
        end if
    end function is_palindromic_int64

    pure recursive function gcd_int32(n1, n2) result(ans)
        integer, intent(in) :: n1, n2
        integer :: ans

        if (n2 == 0) then
            ans = n1
        else
            ans = gcd_int32(n2, mod(n1, n2))
        end if
    end function gcd_int32

    pure recursive function gcd_int64(n1, n2) result(ans)
        integer(int64), intent(in) :: n1, n2
        integer(int64) :: ans

        if (n2 == 0_int64) then
            ans = n1
        else
            ans = gcd_int64(n2, mod(n1, n2))
        end if
    end function gcd_int64

    pure integer function lcm_int32(n1, n2)
        integer, intent(in) :: n1, n2

        lcm_int32 = n1*n2/gcd_int32(n1, n2)
    end function lcm_int32

    pure integer(int64) function lcm_int64(n1, n2)
        integer(int64), intent(in) :: n1, n2

        lcm_int64 = n1*n2/gcd_int64(n1, n2)
    end function lcm_int64

    pure integer function factorial_int32(n)
        integer, intent(in) :: n
        integer :: i, tmp

        tmp = 1
        do i = 1, n
            tmp = tmp*i
        end do
        factorial_int32 = tmp
    end function factorial_int32

    pure integer(int64) function factorial_int64(n)
        integer(int64), intent(in) :: n
        integer(int64) :: i, tmp

        tmp = 1_int64
        do i = 1_int64, n
            tmp = tmp*i
        end do
        factorial_int64 = tmp
    end function factorial_int64

    pure logical function is_pandigital_int32(n, digs)
        integer, intent(in) :: n
        integer, intent(in), optional :: digs
        integer :: tmp, j, l
        logical, allocatable :: logic_arr(:)

        if (present(digs)) then
            allocate (logic_arr(digs))
            l = digs
        else
            l = 9
            allocate (logic_arr(l))
        end if

        is_pandigital_int32 = .false.
        logic_arr = .false.
        tmp = n

        do
            j = unit_digit_int32(tmp)
            if (j == 0 .or. j > l) exit
            logic_arr(j) = .true.
            tmp = tmp/10
        end do

        if (count(logic_arr) == l) then
            is_pandigital_int32 = .true.
        else
            is_pandigital_int32 = .false.
        end if
    end function is_pandigital_int32

    pure logical function is_pandigital_int64(n, digs)
        integer(int64), intent(in) :: n
        integer(int64), intent(in), optional :: digs
        integer(int64) :: tmp, j, l
        logical, allocatable :: logic_arr(:)

        if (present(digs)) then
            allocate (logic_arr(digs))
            l = digs
        else
            l = 9_int64
            allocate (logic_arr(l))
        end if

        is_pandigital_int64 = .false.
        logic_arr = .false.
        tmp = n

        do
            j = unit_digit_int64(tmp)
            if (j == 0_int64 .or. j > l) exit
            logic_arr(j) = .true.
            tmp = tmp/10_int64
        end do

        if (count(logic_arr) == l) then
            is_pandigital_int64 = .true.
        else
            is_pandigital_int64 = .false.
        end if
    end function is_pandigital_int64

    pure subroutine int_2_arr_int32(n, arr)
        integer, intent(in) :: n
        integer, allocatable, intent(out) :: arr(:)
        integer :: tmp, i, l

        tmp = n
        l = digs_of_int_int32(tmp)

        if (l == 1) then
            allocate (arr(1))
            arr(1) = n
            return
        end if

        allocate (arr(l))
        do i = l, 1, -1
            arr(i) = unit_digit_int32(tmp)
            tmp = tmp/10
        end do
    end subroutine int_2_arr_int32

    pure subroutine int_2_arr_int64(n, arr)
        integer(int64), intent(in) :: n
        integer, allocatable, intent(out) :: arr(:)
        integer(int64) :: tmp
        integer :: i, l

        tmp = n
        l = int(digs_of_int_int64(tmp))

        if (l == 1) then
            allocate (arr(1))
            arr(1) = int(n)
            return
        end if

        allocate (arr(l))
        do i = l, 1, -1
            arr(i) = int(unit_digit_int64(tmp))
            tmp = tmp/10_int64
        end do
    end subroutine int_2_arr_int64

    pure subroutine arr_2_int_int32(arr, n)
        integer, intent(in) :: arr(:)
        integer, intent(out) :: n
        integer :: i, tmp, l

        l = size(arr, dim=1)
        tmp = 0

        do i = 1, l
            tmp = tmp*10 + arr(i)
        end do

        n = tmp
    end subroutine arr_2_int_int32

    pure subroutine arr_2_int_int64(arr, n)
        integer(int64), intent(in) :: arr(:)
        integer(int64), intent(out) :: n
        integer(int64) :: i, tmp, l

        l = size(arr, dim=1)
        tmp = 0_int64

        do i = 1_int64, l
            tmp = tmp*10_int64 + arr(i)
        end do

        n = tmp
    end subroutine arr_2_int_int64

    pure subroutine append_sp(arr, e)
        real(sp), allocatable, intent(inout) :: arr(:)
        real(sp), intent(in) :: e

        if (allocated(arr)) then
            arr = [arr, [e]]
        else
            arr = [e]
        end if
    end subroutine append_sp

    pure subroutine append_dp(arr, e)
        real(dp), allocatable, intent(inout) :: arr(:)
        real(dp), intent(in) :: e

        if (allocated(arr)) then
            arr = [arr, [e]]
        else
            arr = [e]
        end if
    end subroutine append_dp

    pure subroutine append_int32(arr, e)
        integer, allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: e

        if (allocated(arr)) then
            arr = [arr, [e]]
        else
            arr = [e]
        end if
    end subroutine append_int32

    pure subroutine append_int64(arr, e)
        integer(int64), allocatable, intent(inout) :: arr(:)
        integer(int64), intent(in) :: e

        if (allocated(arr)) then
            arr = [arr, [e]]
        else
            arr = [e]
        end if
    end subroutine append_int64

    pure subroutine next_permutation_int32(k, n, idx, next_permutation_avail)
        integer, intent(in) :: k, n
        integer, intent(inout) :: idx(k)
        logical, intent(out) :: next_permutation_avail
        logical :: carr(k)
        integer :: i, x, end_arr(k)

        end_arr = [(i, i=n - k + 1, n)]
        next_permutation_avail = .true.
        if (all(idx == end_arr)) then
            next_permutation_avail = .false.
            return
        end if

        carr = .true.
        label_carry: do i = k, 1, -1
            if (idx(i) == n - k + i) carr(i) = .false.
        end do label_carry

        if (all(carr .eqv. .true.)) then
            idx(k) = idx(k) + 1
        else
            x = findloc(carr, value=.false., dim=1) - 1
            idx(x:k) = [(idx(x) + i, i=1, k - x + 1)]
        end if
    end subroutine next_permutation_int32

    pure subroutine next_permutation_int64(k, n, idx, next_permutation_avail)
        integer(int64), intent(in) :: k, n
        integer(int64), intent(inout) :: idx(k)
        logical, intent(out) :: next_permutation_avail
        logical :: carr(k)
        integer(int64) :: i, x, end_arr(k)

        end_arr = [(i, i=n - k + 1, n)]
        next_permutation_avail = .true.
        if (all(idx == end_arr)) then
            next_permutation_avail = .false.
            return
        end if

        carr = .true.
        label_carry: do i = k, 1, -1
            if (idx(i) == n - k + i) carr(i) = .false.
        end do label_carry

        if (all(carr .eqv. .true.)) then
            idx(k) = idx(k) + 1
        else
            x = findloc(carr, value=.false., dim=1) - 1
            idx(x:k) = [(idx(x) + i, i=1, k - x + 1)]
        end if
    end subroutine next_permutation_int64

    pure subroutine next_permutation2_int32(arr, next_permutation_avail)
        integer, intent(inout) :: arr(:)
        logical, intent(out) :: next_permutation_avail
        integer :: i, k, l

        k = 0; l = 0
        do i = size(arr) - 1, 1, -1
            if (arr(i) < arr(i + 1)) then
                k = i
                next_permutation_avail = .true.
                exit
            else
                next_permutation_avail = .false.
            end if
        end do

        do i = size(arr), 1, -1
            if (arr(k) < arr(i)) then
                l = i; exit
            end if
        end do

        call swap(arr(k), arr(l))
        arr(k + 1:size(arr)) = arr(size(arr):k + 1:-1)
    end subroutine next_permutation2_int32

end module euler_utils_m

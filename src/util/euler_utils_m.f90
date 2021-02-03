module euler_utils_m
    use iso_fortran_env, only: int64, real32, real64
    implicit none
    private

    integer, parameter, public :: sp = real32, dp = real64
    real(sp), parameter, public :: tiny_sp = tiny(0._sp)
    real(dp), parameter, public :: tiny_dp = tiny(0._dp)

    public :: unit_digit
    interface unit_digit
        module procedure unit_digit_int32
        module procedure unit_digit_int64
    end interface unit_digit

    public :: swap
    interface swap
        module procedure swap_sp, swap_dp
        module procedure swap_int32, swap_int64
    end interface swap

    public :: digs_of_int
    interface digs_of_int
        module procedure digs_of_int_int32
        module procedure digs_of_int_int64
    end interface digs_of_int

    public :: fibonacci
    interface fibonacci
        module procedure fib32, fib64
    end interface fibonacci

    public :: reverse
    interface reverse
        module procedure reverse_int32, reverse_int64
    end interface reverse

    public :: is_palindromic
    interface is_palindromic
        module procedure is_palindromic_int32
        module procedure is_palindromic_int64
    end interface is_palindromic

    public :: gcd
    interface gcd
        module procedure gcd_int32, gcd_int64
    end interface gcd

    public :: lcm
    interface lcm
        module procedure lcm_int32, lcm_int64
    end interface lcm

    public :: factorial
    interface factorial
        module procedure factorial_int32
        module procedure factorial_int64
    end interface factorial

    public :: is_pandigital
    interface is_pandigital
        module procedure is_pandigital_int32
        module procedure is_pandigital_int64
    end interface is_pandigital

    public :: int_2_arr
    interface int_2_arr
        module procedure int_2_arr_int32
        module procedure int_2_arr_int64
    end interface int_2_arr

    public :: arr_2_int
    interface arr_2_int
        module procedure arr_2_int_int32
        module procedure arr_2_int_int64
    end interface arr_2_int

    public :: append
    interface append
        module procedure append_sp, append_dp
        module procedure append_int32, append_int64
    end interface append

    public :: next_permutation
    interface next_permutation
        module procedure next_permutation_int32
        module procedure next_permutation_int64
    end interface next_permutation

contains

    integer function unit_digit_int32(n)
        integer, intent(in) :: n
        unit_digit_int32 = n - n/10*10
    end function unit_digit_int32

    integer(int64) function unit_digit_int64(n)
        integer(int64), intent(in) :: n
        unit_digit_int64 = n - n/10_int64*10_int64
    end function unit_digit_int64

    subroutine swap_sp(a, b)
        real(sp), intent(inout) :: a, b
        real(sp) :: tmp

        tmp = a; a = b; b = tmp
    end subroutine swap_sp

    subroutine swap_dp(a, b)
        real(dp), intent(inout) :: a, b
        real(dp) :: tmp

        tmp = a; a = b; b = tmp
    end subroutine swap_dp

    subroutine swap_int32(a, b)
        integer, intent(inout) :: a, b
        integer :: tmp

        tmp = a; a = b; b = tmp
    end subroutine swap_int32

    subroutine swap_int64(a, b)
        integer(int64), intent(inout) :: a, b
        integer(int64) :: tmp

        tmp = a; a = b; b = tmp
    end subroutine swap_int64

    integer function digs_of_int_int32(n)
        integer, intent(in) :: n

        digs_of_int_int32 = floor(log10(real(n, sp))) + 1
    end function digs_of_int_int32

    integer(int64) function digs_of_int_int64(n)
        integer(int64), intent(in) :: n

        digs_of_int_int64 = floor(log10(real(n, sp))) + 1_int64
    end function digs_of_int_int64

    recursive function fib32(n) result(ans)
        integer, intent(in) :: n
        integer :: k, ans

        if (n < 0 .or. n > 92) then
            error stop "fib32: Invalid input number."
        end if

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

    recursive function fib64(n) result(ans)
        integer(int64), intent(in) :: n
        integer(int64) :: k, ans

        if (n < 0_int64 .or. n > 92_int64) then
            error stop "fib64: Invalid input number."
        end if

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

    integer function reverse_int32(n)
        integer, intent(in) :: n
        integer :: reversed, tmp

        reversed = 0; tmp = n
        do while (tmp > 0)
            reversed = reversed*10 + mod(tmp, 10)
            tmp = tmp/10
        end do
        reverse_int32 = reversed
    end function reverse_int32

    integer(int64) function reverse_int64(n)
        integer(int64), intent(in) :: n
        integer(int64) :: reversed, tmp
        reversed = 0_int64; tmp = n
        do while (tmp > 0_int64)
            reversed = reversed*10_int64 + mod(tmp, 10_int64)
            tmp = tmp/10_int64
        end do
        reverse_int64 = reversed
    end function reverse_int64

    logical function is_palindromic_int32(n)
        integer, intent(in) :: n

        is_palindromic_int32 = .false.
        if (n == reverse_int32(n)) then
            is_palindromic_int32 = .true.
        end if
    end function is_palindromic_int32

    logical function is_palindromic_int64(n)
        integer(int64), intent(in) :: n

        is_palindromic_int64 = .false.
        if (n == reverse_int64(n)) then
            is_palindromic_int64 = .true.
        end if
    end function is_palindromic_int64

    recursive function gcd_int32(n1, n2) result(ans)
        integer, intent(in) :: n1, n2
        integer :: ans

        if (n2 == 0) then
            ans = n1
            return
        else
            ans = gcd_int32(n2, mod(n1, n2))
            return
        end if
    end function gcd_int32

    recursive function gcd_int64(n1, n2) result(ans)
        integer(int64), intent(in) :: n1, n2
        integer(int64) :: ans

        if (n2 == 0_int64) then
            ans = n1
            return
        else
            ans = gcd_int64(n2, mod(n1, n2))
            return
        end if
    end function gcd_int64

    integer function lcm_int32(n1, n2)
        integer, intent(in) :: n1, n2

        lcm_int32 = n1*n2/gcd_int32(n1, n2)
    end function lcm_int32

    integer(int64) function lcm_int64(n1, n2)
        integer(int64), intent(in) :: n1, n2

        lcm_int64 = n1*n2/gcd_int64(n1, n2)
    end function lcm_int64

    integer function factorial_int32(n)
        integer, intent(in) :: n
        integer :: i, tmp

        if (n >= 13) then
            error stop "FACTORIAL_INT32: n >= 13."
        end if

        tmp = 1
        do i = 1, n
            tmp = tmp*i
        end do
        factorial_int32 = tmp
    end function factorial_int32

    integer(int64) function factorial_int64(n)
        integer(int64), intent(in) :: n
        integer(int64) :: i, tmp

        if (n >= 13_int64) then
            error stop "FACTORIAL_INT32: n >= 13."
        end if

        tmp = 1_int64
        do i = 1_int64, n
            tmp = tmp*i
        end do
        factorial_int64 = tmp
    end function factorial_int64

    logical function is_pandigital_int32(n, digs)
        integer, intent(in) :: n
        integer, intent(in), optional :: digs
        integer :: tmp, j, l
        logical, allocatable, dimension(:) :: logic_arr

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

    logical function is_pandigital_int64(n, digs)
        integer(int64), intent(in) :: n
        integer(int64), intent(in), optional :: digs
        integer(int64) :: tmp, j, l
        logical, allocatable, dimension(:) :: logic_arr

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

    subroutine int_2_arr_int32(n, arr)
        integer, intent(in) :: n
        integer, allocatable, dimension(:), intent(out) :: arr
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

    subroutine int_2_arr_int64(n, arr)
        integer(int64), intent(in) :: n
        integer, allocatable, dimension(:), intent(out) :: arr
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

    subroutine arr_2_int_int32(arr, n)
        integer, dimension(:), intent(in) :: arr
        integer, intent(out) :: n
        integer :: i, tmp, l

        l = size(arr, dim=1)
        tmp = 0

        do i = 1, l
            tmp = tmp*10 + arr(i)
        end do

        n = tmp
    end subroutine arr_2_int_int32

    subroutine arr_2_int_int64(arr, n)
        integer(int64), dimension(:), intent(in) :: arr
        integer(int64), intent(out) :: n
        integer(int64) :: i, tmp, l

        l = size(arr, dim=1)
        tmp = 0_int64

        do i = 1_int64, l
            tmp = tmp*10_int64 + arr(i)
        end do

        n = tmp
    end subroutine arr_2_int_int64

    subroutine append_sp(arr, e)
        real(sp), allocatable, dimension(:), intent(inout) :: arr
        real(sp), intent(in) :: e
        real(sp), allocatable, dimension(:) :: tmp

        if (allocated(arr)) then
            call move_alloc(arr, tmp)
            allocate (arr(size(tmp) + 1))
            arr(1:size(tmp)) = tmp(:)
            arr(size(tmp) + 1) = e
        else
            allocate (arr(1))
            arr(1) = e
        end if
    end subroutine append_sp

    subroutine append_dp(arr, e)
        real(dp), allocatable, dimension(:), intent(inout) :: arr
        real(dp), intent(in) :: e
        real(dp), allocatable, dimension(:) :: tmp

        if (allocated(arr)) then
            call move_alloc(arr, tmp)
            allocate (arr(size(tmp) + 1))
            arr(1:size(tmp)) = tmp(:)
            arr(size(tmp) + 1) = e
        else
            allocate (arr(1))
            arr(1) = e
        end if
    end subroutine append_dp

    subroutine append_int32(arr, e)
        integer, allocatable, dimension(:), intent(inout) :: arr
        integer, intent(in) :: e
        integer, allocatable, dimension(:) :: tmp

        if (allocated(arr)) then
            call move_alloc(arr, tmp)
            allocate (arr(size(tmp) + 1))
            arr(1:size(tmp)) = tmp(:)
            arr(size(tmp) + 1) = e
        else
            allocate (arr(1))
            arr(1) = e
        end if
    end subroutine append_int32

    subroutine append_int64(arr, e)
        integer(int64), allocatable, dimension(:), intent(inout) :: arr
        integer(int64), intent(in) :: e
        integer(int64), allocatable, dimension(:) :: tmp

        if (allocated(arr)) then
            call move_alloc(arr, tmp)
            allocate (arr(size(tmp) + 1))
            arr(1:size(tmp)) = tmp(:)
            arr(size(tmp) + 1) = e
        else
            allocate (arr(1))
            arr(1) = e
        end if
    end subroutine append_int64

    function next_permutation_int32(k, n, idx) result(ret)
        integer, intent(in) :: k, n
        integer, intent(inout) :: idx(k)
        logical :: ret, carr(k)
        integer :: i, x, end_arr(k)

        end_arr = [(i, i=n - k + 1, n)]
        ret = .true.
        if (all(idx == end_arr)) then
            ret = .false.
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
    end function next_permutation_int32

    function next_permutation_int64(k, n, idx) result(ret)
        integer(int64), intent(in) :: k, n
        integer(int64), intent(inout) :: idx(k)
        logical :: ret, carr(k)
        integer(int64) :: i, x, end_arr(k)

        end_arr = [(i, i=n - k + 1, n)]
        ret = .true.
        if (all(idx == end_arr)) then
            ret = .false.
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
    end function next_permutation_int64

end module euler_utils_m

module euler_multiprecision_util_m

    implicit none
    private
    public :: add, sub, compare, mul, pow2

contains

    function cut_leading_zeros(arr) result(ret)
        integer, allocatable, intent(in) :: arr(:)
        integer, allocatable :: ret(:)
        integer :: i

        i = 1
        find_zeros: do
            if (arr(i) /= 0 .or. i >= size(arr)) exit find_zeros
            i = i + 1
        end do find_zeros
        ret = arr(i:)
    end function cut_leading_zeros

    function carry(arr) result(ret)
        integer, intent(in) :: arr(:)
        integer, allocatable :: tmp1(:), tmp2(:), ret(:)

        associate (x => size(arr))
            allocate (tmp1(x + 2), tmp2(x + 2), ret(x + 2))
        end associate
        tmp1 = 0; tmp2 = 0; ret(1:2) = 0; ret(3:) = arr(:)

        do
            if (all(ret <= 9)) exit
            tmp1(:) = ret(:) - ret(:)/10*10
            tmp2(1:size(tmp2) - 1) = ret(2:)/10
            ret(:) = tmp1(:) + tmp2(:)
        end do
    end function carry

    function add(arr1, arr2) result(ret)
        integer, allocatable, intent(in) :: arr1(:), arr2(:)
        integer, allocatable, dimension(:) :: ret, tmp1, tmp2

        associate (x => max(size(arr1), size(arr2)) + 1)
            allocate (tmp1(x), tmp2(x))
            tmp1 = 0; tmp1(x - size(arr1) + 1:x) = arr1(:)
            tmp2 = 0; tmp2(x - size(arr2) + 1:x) = arr2(:)
        end associate
        ret = cut_leading_zeros(carry(tmp1 + tmp2))
    end function add

    function sub(arr1, arr2) result(ret)
        integer, allocatable, intent(in) :: arr1(:), arr2(:)
        integer, allocatable, dimension(:) :: ret, tmp1, tmp2, tmp
        integer :: i

        associate (x => max(size(arr1), size(arr2)) + 1)
            allocate (tmp1(x), tmp2(x), tmp(x))
            tmp1 = 0; tmp2 = 0; tmp = 0
            tmp1(x - size(arr1) + 1:x) = arr1(:)
            tmp2(x - size(arr2) + 1:x) = arr2(:)
        end associate

        do i = size(tmp1), 2, -1
            if (tmp1(i) >= tmp2(i)) then
                tmp(i) = tmp1(i) - tmp2(i)
            else
                tmp(i) = tmp1(i) - tmp2(i) + 10
                tmp1(i - 1) = tmp1(i - 1) - 1
            end if
        end do
        ret = cut_leading_zeros(tmp)
    end function sub

    function compare(arr1, arr2) result(ret)
        integer, allocatable, intent(in) :: arr1(:), arr2(:)
        integer :: ret, i

        if (size(arr1) > size(arr2)) then
            ret = 1; return
        else if (size(arr1) < size(arr2)) then
            ret = -1; return
        end if

        do i = size(arr1), 1, -1
            if (arr1(i) > arr2(i)) then
                ret = 1; return
            else if (arr1(i) < arr2(i)) then
                ret = -1; return
            else
                ret = 0
            end if
        end do
        if (ret /= 0) error stop 'compare: invalid output.'
    end function compare

    function mul(arr1, arr2) result(ret)
        integer, intent(in) :: arr1(:), arr2(:)
        integer, allocatable :: tmp(:), tmp_row(:), ret(:)
        integer :: i

        associate (size1 => size(arr1), size2 => size(arr2), &
                   tot => size(arr1) + size(arr2))
            allocate (tmp_row(tot), tmp(tot + 2))
            tmp = 0

            do i = 1, size2
                associate (x => tot - i + 1)
                    tmp_row = 0; tmp_row(x - size1 + 1:x) = arr1
                end associate
                tmp = tmp + carry(tmp_row*arr2(size2 - i + 1))
            end do
        end associate
        ret = cut_leading_zeros(carry(tmp))
    end function mul

    recursive function pow2(arr, n) result(ret)
        integer, intent(in) :: arr(:)
        integer, intent(in) :: n
        integer, allocatable :: ret(:)

        if (n < 0) error stop 'pow2: n must be positive.'

        if (n == 0) then
            ret = [1]
        else if (n == 1) then
            ret = arr
        else if (mod(n, 2) == 0) then
            ret = pow2(mul(arr, arr), n/2)
        else if (mod(n, 2) /= 0) then
            ret = pow2(mul(arr, arr), (n - 1)/2)
            ret = mul(arr, ret)
        end if
    end function pow2

end module euler_multiprecision_util_m

module euler_multiprecision_m

    use euler_multiprecision_util_m
    implicit none
    private

    type, public :: multiprecision_int_t
        integer, allocatable :: arr(:)
        character(len=1) :: sgn
    contains
        procedure :: re_alloc => re_alloc_sub
        procedure, private :: init_char_sub, init_int_sub, init_arr_sub
        generic :: assignment(=) => init_char_sub, init_int_sub, init_arr_sub
        procedure, private :: eq_func, eq_int_func, eq_char_func
        generic :: operator(==) => eq_func, eq_int_func, eq_char_func
        procedure, private :: gt_func, gt_int_func, gt_char_func
        generic :: operator(>) => gt_func, gt_int_func, gt_char_func
        procedure, private :: lt_func, lt_int_func, lt_char_func
        generic :: operator(<) => lt_func, lt_int_func, lt_char_func
        procedure, private :: ge_func, ge_int_func, ge_char_func
        generic :: operator(>=) => ge_func, ge_int_func, ge_char_func
        procedure, private :: le_func, le_int_func, le_char_func
        generic :: operator(<=) => le_func, le_int_func, le_char_func
        procedure, private :: add_func, add_int_func, add_char_func
        generic :: operator(+) => add_func, add_int_func, add_char_func
        procedure, private :: sub_func, sub_int_func, sub_char_func
        generic :: operator(-) => sub_func, sub_int_func, sub_char_func
        procedure, private :: mul_func, mul_int_func, mul_char_func
        generic :: operator(*) => mul_func, mul_int_func, mul_char_func
        ! procedure, private :: div_func, div_int_func, div_char_func
        ! generic :: operator(/) => div_func, div_int_func, div_char_func
        procedure, private :: pow_int_func
        generic :: operator(**) => pow_int_func
        ! procedure, private :: fac_func, fac_int_func, fac_char_func
        ! generic :: operator(.fac.) => fac_func, fac_int_func, fac_char_func
    end type multiprecision_int_t

    public :: to_long
    interface to_long
        module procedure to_long_char, to_long_int
    end interface

    !   ! TODO-list
    !   ! For a multiprecision_int_t, once the type is declared
    !   type(multiprecision_int_t) :: val1, val2, val3
    !
    !   ! One should be able to:
    !   ! 1. print the value
    !   call val1%print(val_only=.false., sgn_only=.false.)
    !   ! 2. floor division
    !   val3 = val1 / val2
    !   ! 3. power (partially done)
    !   val3 = val1**val2
    !   ! 4. factorial
    !   val2 = .fac. val1

contains

    subroutine re_alloc_sub(this, n)
        class(multiprecision_int_t), intent(inout) :: this
        integer, intent(in) :: n

        if (allocated(this%arr)) deallocate (this%arr)
        allocate (this%arr(n))
    end subroutine re_alloc_sub

    subroutine init_char_sub(this, chars)
        class(multiprecision_int_t), intent(inout) :: this
        character(len=*), intent(in) :: chars
        integer :: i

        select case (chars(1:1))
        case ('+')
            this%sgn = '+'
            call this%re_alloc(len(chars) - 1)
            do i = 2, len(chars)
                read (chars(i:i), *) this%arr(i - 1)
            end do
        case ('-')
            this%sgn = '-'
            call this%re_alloc(len(chars) - 1)
            do i = 2, len(chars)
                read (chars(i:i), *) this%arr(i - 1)
            end do
        case default
            this%sgn = '+'
            call this%re_alloc(len(chars))
            do i = 1, len(chars)
                read (chars(i:i), *) this%arr(i)
            end do
        end select
    end subroutine init_char_sub

    subroutine init_arr_sub(this, arr)
        class(multiprecision_int_t), intent(inout) :: this
        integer, allocatable, intent(in) :: arr(:)

        this%arr = arr(:)
        this%sgn = '+'
    end subroutine init_arr_sub

    subroutine init_int_sub(this, val)
        class(multiprecision_int_t), intent(inout) :: this
        integer, intent(in) :: val
        integer :: tmp, i, digs

        if (val >= 0) then
            this%sgn = '+'
        else
            this%sgn = '-'
        end if

        tmp = abs(val)
        digs = floor(log10(real(tmp))) + 1
        call this%re_alloc(digs)

        do i = digs, 1, -1
            this%arr(i) = int(mod(tmp, 10))
            tmp = tmp/10
        end do
    end subroutine init_int_sub

    function to_long_char(chars) result(ret)
        character(len=*), intent(in) :: chars
        type(multiprecision_int_t) :: ret

        ret = chars
    end function to_long_char

    function to_long_int(val) result(ret)
        integer, intent(in) :: val
        type(multiprecision_int_t) :: ret

        ret = val
    end function to_long_int

    logical function eq_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        type(multiprecision_int_t), intent(in) :: val

        eq_func = .false.
        if (compare(this%arr, val%arr) == 0 .and. &
            this%sgn == val%sgn) eq_func = .true.
    end function eq_func

    logical function eq_int_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        integer, intent(in) :: val

        eq_int_func = this%eq_func(to_long(val))
    end function eq_int_func

    logical function eq_char_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        character(len=*), intent(in) :: val

        eq_char_func = this%eq_func(to_long(val))
    end function eq_char_func

    logical function gt_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        type(multiprecision_int_t), intent(in) :: val

        gt_func = .false.
        if (this%sgn == '+' .and. val%sgn == '-') then
            gt_func = .true.
        else if (compare(this%arr, val%arr) == 1 .and. &
                 this%sgn == '+' .and. val%sgn == '+') then
            gt_func = .true.
        else if (compare(this%arr, val%arr) == -1 .and. &
                 this%sgn == '-' .and. val%sgn == '-') then
            gt_func = .true.
        end if
    end function gt_func

    logical function gt_int_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        integer, intent(in) :: val

        gt_int_func = this%gt_func(to_long(val))
    end function gt_int_func

    logical function gt_char_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        character(len=*), intent(in) :: val

        gt_char_func = this%gt_func(to_long(val))
    end function gt_char_func

    logical function lt_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        type(multiprecision_int_t), intent(in) :: val

        lt_func = .false.
        if (this%sgn == '-' .and. val%sgn == '+') then
            lt_func = .true.
        else if (compare(this%arr, val%arr) == -1 .and. &
                 this%sgn == '+' .and. val%sgn == '+') then
            lt_func = .true.
        else if (compare(this%arr, val%arr) == 1 .and. &
                 this%sgn == '-' .and. val%sgn == '-') then
            lt_func = .true.
        end if
    end function lt_func

    logical function lt_int_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        integer, intent(in) :: val

        lt_int_func = this%lt_func(to_long(val))
    end function lt_int_func

    logical function lt_char_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        character(len=*), intent(in) :: val

        lt_char_func = this%lt_func(to_long(val))
    end function lt_char_func

    logical function ge_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        type(multiprecision_int_t), intent(in) :: val

        ge_func = .false.
        if (this%gt_func(val) .or. this%eq_func(val)) ge_func = .true.
    end function ge_func

    logical function ge_int_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        integer, intent(in) :: val

        ge_int_func = this%ge_func(to_long(val))
    end function ge_int_func

    logical function ge_char_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        character(len=*), intent(in) :: val

        ge_char_func = this%ge_func(to_long(val))
    end function ge_char_func

    logical function le_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        type(multiprecision_int_t), intent(in) :: val

        le_func = .false.
        if (this%lt_func(val) .or. this%eq_func(val)) le_func = .true.
    end function le_func

    logical function le_int_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        integer, intent(in) :: val

        le_int_func = this%le_func(to_long(val))
    end function le_int_func

    logical function le_char_func(this, val)
        class(multiprecision_int_t), intent(in) :: this
        character(len=*), intent(in) :: val

        le_char_func = this%le_func(to_long(val))
    end function le_char_func

    function add_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        type(multiprecision_int_t), intent(in) :: val
        type(multiprecision_int_t) :: ret

        if (compare(this%arr, val%arr) == 0 .and. &
            this%sgn /= val%sgn) then
            ret = '0'
        else if (this%sgn == '+' .and. val%sgn == '+') then
            ret%arr = add(this%arr, val%arr)
            ret%sgn = '+'
        else if (this%sgn == '-' .and. val%sgn == '-') then
            ret%arr = add(this%arr, val%arr)
            ret%sgn = '-'
        else if (this%sgn == '+' .and. val%sgn == '-') then
            if (compare(this%arr, val%arr) == 1) then
                ret%arr = sub(this%arr, val%arr)
                ret%sgn = '+'
            else
                ret%arr = sub(val%arr, this%arr)
                ret%sgn = '-'
            end if
        else if (this%sgn == '-' .and. val%sgn == '+') then
            if (compare(this%arr, val%arr) == 1) then
                ret%arr = sub(this%arr, val%arr)
                ret%sgn = '-'
            else
                ret%arr = sub(val%arr, this%arr)
                ret%sgn = '+'
            end if
        end if
    end function

    function add_int_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        integer, intent(in) :: val
        type(multiprecision_int_t) :: ret

        ret = this%add_func(to_long(val))
    end function add_int_func

    function add_char_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        character(len=*), intent(in) :: val
        type(multiprecision_int_t) :: ret

        ret = this%add_func(to_long(val))
    end function add_char_func

    function sub_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        type(multiprecision_int_t), intent(in) :: val
        type(multiprecision_int_t) :: ret

        if (this == val) then
            ret = '0'
        else if (this%sgn == '+' .and. val%sgn == '+') then
            if (compare(this%arr, val%arr) == 1) then
                ret%arr = sub(this%arr, val%arr)
                ret%sgn = '+'
            else
                ret%arr = sub(val%arr, this%arr)
                ret%sgn = '-'
            end if
        else if (this%sgn == '+' .and. val%sgn == '-') then
            ret%arr = add(this%arr, val%arr)
            ret%sgn = '+'
        else if (this%sgn == '-' .and. val%sgn == '+') then
            ret%arr = add(this%arr, val%arr)
            ret%sgn = '-'
        else if (this%sgn == '-' .and. val%sgn == '-') then
            if (compare(this%arr, val%arr) == 1) then
                ret%arr = sub(this%arr, val%arr)
                ret%sgn = '-'
            else
                ret%arr = sub(val%arr, this%arr)
                ret%sgn = '+'
            end if
        end if
    end function sub_func

    function sub_int_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        integer, intent(in) :: val
        type(multiprecision_int_t) :: ret

        ret = this%sub_func(to_long(val))
    end function sub_int_func

    function sub_char_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        character(len=*), intent(in) :: val
        type(multiprecision_int_t) :: ret

        ret = this%sub_func(to_long(val))
    end function sub_char_func

    function mul_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        type(multiprecision_int_t), intent(in) :: val
        type(multiprecision_int_t) :: ret

        if (this == 0 .or. val == 0) then
            ret%sgn = '+'
            ret%arr = [0]
        else if (this%sgn == val%sgn) then
            ret%sgn = '+'
            ret%arr = mul(this%arr, val%arr)
        else if (this%sgn /= val%sgn) then
            ret%sgn = '-'
            ret%arr = mul(this%arr, val%arr)
        end if
    end function mul_func

    function mul_int_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        integer, intent(in) :: val
        type(multiprecision_int_t) :: ret

        ret = this%mul_func(to_long(val))
    end function mul_int_func

    function mul_char_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        character(len=*), intent(in) :: val
        type(multiprecision_int_t) :: ret

        ret = this%mul_func(to_long(val))
    end function mul_char_func

    function pow_int_func(this, val) result(ret)
        class(multiprecision_int_t), intent(in) :: this
        integer, intent(in) :: val
        type(multiprecision_int_t) :: ret

        if (this%sgn == '+') then
            ret%sgn = '+'
            ret%arr = pow2(this%arr, val)
        else
            if (mod(val, 2) == 0) then
                ret%sgn = '+'
            else if (mod(val, 2) /= 0) then
                ret%sgn = '-'
            end if
            ret%arr = pow2(this%arr, val)
        end if
    end function pow_int_func

end module euler_multiprecision_m
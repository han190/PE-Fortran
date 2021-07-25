module euler_multiprecision_m

    use euler_multiprecision_util_m
    implicit none
    private

    type, public :: multiprecision_int_t
        !! Muliple precision integer type for Project Euler
        !!
        !!### Usage
        !!#### Addition
        !!``` fortran
        !!  type(multiprecision_int_t) :: a, b, c
        !!  integer(int32) :: d
        !!  integer(int32), dimension(4) :: e
        !!
        !!  a = '29348579238475928347592345'
        !!  b = '49587394758345983423486928347'
        !!  ! Add two multiprecision integer types together
        !!  c = a + b
        !!  ! Add a multiprecision integer type with an 32bit integer
        !!  ! (every time the assignment '=' appears the allocatable array
        !!  ! will be re-allocated)
        !!  d = 2394
        !!  c = a + d
        !!  ! One could also do something like the following, the array
        !!  ! will be treated like an integer.
        !!  e = [2, 3, 9, 4]
        !!  c = a + e
        !!```
        !!
        !!#### Subtraction
        !!``` fortran
        !!  type(multiprecision_int_t) :: a, b, c
        !!  a = '29348579238475928347592345'
        !!  b = '49587394758345983423486928347'
        !!
        !!  c = a - b
        !!```
        !!
        !!#### Multiplication
        !!``` fortran
        !!  type(multiprecision_int_t) :: a, b, c
        !!  a = '29348579238475928347592345'
        !!  b = '49587394758345983423486928347'
        !!
        !!  c = a*b
        !!```
        !!
        !!#### Power
        !!``` fortran
        !!  type(multiprecision_int_t) :: a, b
        !!
        !!  b = a**2345
        !!```
        !!
        !!#### Comparison
        !!``` fortran
        !!  type(multiprecision_int_t) :: a, b, c
        !!
        !!  a = '29348579238475928347592345'
        !!  b = '49587394758345983423486928347'
        !!  print *, a >= b, a == b, a <= b
        !!```
        !!
        !!### To-do list
        !! * Print the value
        !! * Floor division (for Miller-rabin)
        !! * Power (Partially done)
        !! * Factorial
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
        !! A generic interface that converts an integer or a string 
        !! into a multiple precision integer type.
        !!
        !!### Usage
        !!```fortran
        !!  type(multiprecision_int_t) :: a, b
        !!  a = '23405982034958034850495098430294850293485'
        !!  b = a + to_long('93845734958')
        !!```
        module procedure to_long_char, to_long_int
    end interface

    public :: digits_of
    interface digits_of
        !! A generic interface that returns the length(digits) of a multiple
        !! precision integer, essentially that is
        !!
        !!### Usage
        !!```fortran
        !!  type(multiprecision_int_t) :: a
        !!  a = '23405982034958034850495098430294850293485'
        !!  print *, size(a%arr) == digits_of(a) ! T
        !!```
        module procedure digits_of_func
    end interface digits_of

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

    function digits_of_func(val) result(ret)
        type(multiprecision_int_t), intent(in) :: val
        integer :: ret

        ret = size(val%arr)
    end function digits_of_func

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

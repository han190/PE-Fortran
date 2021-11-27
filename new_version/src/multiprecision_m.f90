module multiprecision_m

    use multiprecision_utility_m
    use constant_m, only: i32
    implicit none
    private

    !> Muliple precision integer type for Project Euler
    !>
    !>### Usage
    !>#### Addition/Subtraction
    !>``` fortran
    !>program main
    !>    use euler_multiprecision_m
    !>    implicit none
    !>
    !>    type(multiprecision_t) :: a, b, c
    !>    integer :: d
    !>    integer :: e(4)
    !>
    !>    ! Initialization
    !>    ! a = [2, 3, 4, 5, 6, 7, 8]
    !>    a = '29348579238475928347592345'
    !>    b = '49587394758345983423486928347'
    !>    ! Add two multiprecision integer types together
    !>    c = a + b ! or c = a - b
    !>    ! Add a multiprecision integer type with an 32bit integer
    !>    ! (every time the assignment '=' appears the allocatable array
    !>    ! will be re-allocated)
    !>    d = 2394
    !>    c = a + d ! or c = a - d
    !>    ! One could also do something like the following, the array
    !>    ! will be treated like an integer.
    !>    e = [2, 3, 9, 4]
    !>    c = a + e
    !>end program main
    !>```
    !>
    !>#### Multiplication
    !>``` fortran
    !>program main
    !>    use euler_multiprecision_m
    !>    implicit none
    !>
    !>    type(multiprecision_t) :: a, b, c
    !>    integer :: d
    !>
    !>    a = '29348579238475928347592345'
    !>    b = '49587394758345983423486928347'
    !>    d = 40
    !>
    !>    c = a*b
    !>    c = a*d
    !>end program main
    !>```
    !>
    !>#### Power
    !>``` fortran
    !>program main
    !>    use euler_multiprecision_m
    !>    implicit none
    !>
    !>    type(multiprecision_t) :: a, b
    !>
    !>    b = a**2345
    !>end program main
    !>```
    !>
    !>#### Comparison
    !>``` fortran
    !>program main
    !>    use euler_multiprecision_m
    !>    implicit none
    !>
    !>    type(multiprecision_t) :: a, b
    !>    integer :: c
    !>
    !>    a = '29348579238475928347592345'
    !>    b = '49587394758345983423486928347'
    !>    c = 34567
    !>    print *, a >= b, a == b, a <= b
    !>    print *, a >= c, a == c, a <= c
    !>end program main
    !>```
    !>
    !>### To-do list
    !> * Print the value
    !> * Floor division (for Miller-rabin)
    !> * Power (Partially done)
    !> * Factorial
    type, public :: multiprecision_t
        integer(i32), allocatable :: arr(:)
        character(len=1) :: sgn
    contains
        procedure :: re_alloc => re_alloc_sub
        procedure, private :: init_sub, init_char_sub, &
            init_int_sub, init_arr_sub
        generic :: assignment(=) => init_sub, init_char_sub, &
            init_int_sub, init_arr_sub
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
    end type multiprecision_t

    !> A generic interface that converts an integer or a string
    !> into a multiple precision integer type.
    !>
    !>### Usage
    !>```fortran
    !>program main
    !>    use euler_multiprecision_m
    !>    implicit none
    !>
    !>    type(multiprecision_t) :: a, b
    !>    a = '23405982034958034850495098430294850293485'
    !>    b = a + to_long('93845734958')
    !>    b = a + to_long(1234)
    !>end program main
    !>```
    public :: to_long
    interface to_long
        module procedure to_long_char, to_long_int
    end interface

contains

    !> Reallocate a `multiprecision_t`.
    !> This subroutine prevents unnecessary deallocations and re-allocations
    !> of `self%arr`. This is particularly useful when `=` appears in a loop.
    pure subroutine re_alloc_sub(self, n)
        class(multiprecision_t), intent(inout) :: self
        integer(i32), intent(in) :: n

        if (allocated(self%arr)) then
            if (size(self%arr) /= n) then
                deallocate (self%arr)
                allocate (self%arr(n))
            end if
        else
            allocate (self%arr(n))
        end if
    end subroutine re_alloc_sub

    !> Initialize a `multiprecision_t` to a `multiprecision_t`
    pure subroutine init_sub(self, val)
        class(multiprecision_t), intent(inout) :: self
        type(multiprecision_t), intent(in) :: val

        call self%re_alloc(size(val%arr))
        self%sgn = val%sgn
        self%arr = val%arr
    end subroutine init_sub

    !> Initialize a character type to a `multiprecision_t`.
    pure subroutine init_char_sub(self, chars)
        class(multiprecision_t), intent(inout) :: self
        character(len=*), intent(in) :: chars
        integer(i32) :: i

        select case (chars(1:1))
        case ('+')
            self%sgn = '+'
            call self%re_alloc(len(chars) - 1)
            do i = 2, len(chars)
                read (chars(i:i), *) self%arr(i - 1)
            end do
        case ('-')
            self%sgn = '-'
            call self%re_alloc(len(chars) - 1)
            do i = 2, len(chars)
                read (chars(i:i), *) self%arr(i - 1)
            end do
        case default
            self%sgn = '+'
            call self%re_alloc(len(chars))
            do i = 1, len(chars)
                read (chars(i:i), *) self%arr(i)
            end do
        end select
    end subroutine init_char_sub

    !> Initialize an integer array to a `multiprecision_t`.
    pure subroutine init_arr_sub(self, arr)
        class(multiprecision_t), intent(inout) :: self
        integer(i32), allocatable, intent(in) :: arr(:)

        self%arr = arr(:)
        self%sgn = '+'
    end subroutine init_arr_sub

    !> Initialize an integer to a `multiprecision_t`.
    pure subroutine init_int_sub(self, val)
        class(multiprecision_t), intent(inout) :: self
        integer(i32), intent(in) :: val
        integer(i32) :: tmp, i, digs

        if (val >= 0) then
            self%sgn = '+'
        else
            self%sgn = '-'
        end if

        tmp = abs(val)
        digs = floor(log10(real(tmp))) + 1
        call self%re_alloc(digs)

        do i = digs, 1, -1
            self%arr(i) = int(mod(tmp, 10))
            tmp = tmp/10
        end do
    end subroutine init_int_sub

    !> Convert a character type to a `multiprecision_t`.
    pure function to_long_char(chars) result(ret)
        character(len=*), intent(in) :: chars
        type(multiprecision_t) :: ret

        ret = chars
    end function to_long_char

    !> Convert an integer type to a `multiprecision_t`.
    pure function to_long_int(val) result(ret)
        integer(i32), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = val
    end function to_long_int

    !> To judge whether two `multiprecision_t`s are equal.
    pure logical function eq_func(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        eq_func = .false.
        if (compare(self%arr, val%arr) == 0 .and. &
            self%sgn == val%sgn) eq_func = .true.
    end function eq_func

    !> To judge whether a `multiprecision_t` and an integer are equal.
    pure logical function eq_int_func(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        eq_int_func = self%eq_func(to_long(val))
    end function eq_int_func

    !> To judge whether a `multiprecision_t` and an character are equal.
    pure logical function eq_char_func(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        eq_char_func = self%eq_func(to_long(val))
    end function eq_char_func

    !> To judge whether a `multiprecision_t` is
    !> greater than a `multiprecision_t`.
    pure logical function gt_func(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        gt_func = .false.
        if (self%sgn == '+' .and. val%sgn == '-') then
            gt_func = .true.
        else if (compare(self%arr, val%arr) == 1 .and. &
                 self%sgn == '+' .and. val%sgn == '+') then
            gt_func = .true.
        else if (compare(self%arr, val%arr) == -1 .and. &
                 self%sgn == '-' .and. val%sgn == '-') then
            gt_func = .true.
        end if
    end function gt_func

    !> To judge whether a `multiprecision_t` is
    !> greater than an integer.
    pure logical function gt_int_func(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        gt_int_func = self%gt_func(to_long(val))
    end function gt_int_func

    !> To judge whether a `multiprecision_t` is
    !> greather than an character.
    pure logical function gt_char_func(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        gt_char_func = self%gt_func(to_long(val))
    end function gt_char_func

    !> To judge whether a `multiprecision_t` is
    !> less than a `multiprecision_t`.
    pure logical function lt_func(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        lt_func = .false.
        if (self%sgn == '-' .and. val%sgn == '+') then
            lt_func = .true.
        else if (compare(self%arr, val%arr) == -1 .and. &
                 self%sgn == '+' .and. val%sgn == '+') then
            lt_func = .true.
        else if (compare(self%arr, val%arr) == 1 .and. &
                 self%sgn == '-' .and. val%sgn == '-') then
            lt_func = .true.
        end if
    end function lt_func

    !> To judge whether a `multiprecision_t` is
    !> less than an integer.
    pure logical function lt_int_func(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        lt_int_func = self%lt_func(to_long(val))
    end function lt_int_func

    !> To judge whether a `multiprecision_t` is
    !> less than a character.
    pure logical function lt_char_func(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        lt_char_func = self%lt_func(to_long(val))
    end function lt_char_func

    !> To judge whether a `multiprecision_t` is
    !> greater than or equal to a `multiprecision_t`.
    pure logical function ge_func(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        ge_func = .false.
        if (self%gt_func(val) .or. self%eq_func(val)) ge_func = .true.
    end function ge_func

    !> To judge whether a `multiprecision_t` is
    !> greater than or equal to a `multiprecision_t`.
    pure logical function ge_int_func(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        ge_int_func = self%ge_func(to_long(val))
    end function ge_int_func

    !> To judge whether a `multiprecision_t` is
    !> greater than or equal to a character.
    pure logical function ge_char_func(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        ge_char_func = self%ge_func(to_long(val))
    end function ge_char_func

    !> To judge whether a `multiprecision_t` is
    !> less than or equal to a `multiprecision_t`.
    pure logical function le_func(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        le_func = .false.
        if (self%lt_func(val) .or. self%eq_func(val)) le_func = .true.
    end function le_func

    !> To judge whether a `multiprecision_t` is
    !> less than or equal to a integer.
    pure logical function le_int_func(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        le_int_func = self%le_func(to_long(val))
    end function le_int_func

    !> To judge whether a `multiprecision_t` is
    !> less than or equal to a character.
    pure logical function le_char_func(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        le_char_func = self%le_func(to_long(val))
    end function le_char_func

    !> Add two `multiprecision_t`s.
    pure function add_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val
        type(multiprecision_t) :: ret

        if (compare(self%arr, val%arr) == 0 .and. &
            self%sgn /= val%sgn) then
            ret = '0'
        else if (self%sgn == '+' .and. val%sgn == '+') then
            ret%arr = add(self%arr, val%arr)
            ret%sgn = '+'
        else if (self%sgn == '-' .and. val%sgn == '-') then
            ret%arr = add(self%arr, val%arr)
            ret%sgn = '-'
        else if (self%sgn == '+' .and. val%sgn == '-') then
            if (compare(self%arr, val%arr) == 1) then
                ret%arr = sub(self%arr, val%arr)
                ret%sgn = '+'
            else
                ret%arr = sub(val%arr, self%arr)
                ret%sgn = '-'
            end if
        else if (self%sgn == '-' .and. val%sgn == '+') then
            if (compare(self%arr, val%arr) == 1) then
                ret%arr = sub(self%arr, val%arr)
                ret%sgn = '-'
            else
                ret%arr = sub(val%arr, self%arr)
                ret%sgn = '+'
            end if
        end if
    end function

    !> Add a `multiprecision_t` and an integer.
    pure function add_int_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%add_func(to_long(val))
    end function add_int_func

    !> Add a `multiprecision_t` and a character.
    pure function add_char_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%add_func(to_long(val))
    end function add_char_func

    !> Substract a `multiprecision_t` by a `multiprecision_t`.
    pure function sub_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val
        type(multiprecision_t) :: ret

        if (self == val) then
            ret = '0'
        else if (self%sgn == '+' .and. val%sgn == '+') then
            if (compare(self%arr, val%arr) == 1) then
                ret%arr = sub(self%arr, val%arr)
                ret%sgn = '+'
            else
                ret%arr = sub(val%arr, self%arr)
                ret%sgn = '-'
            end if
        else if (self%sgn == '+' .and. val%sgn == '-') then
            ret%arr = add(self%arr, val%arr)
            ret%sgn = '+'
        else if (self%sgn == '-' .and. val%sgn == '+') then
            ret%arr = add(self%arr, val%arr)
            ret%sgn = '-'
        else if (self%sgn == '-' .and. val%sgn == '-') then
            if (compare(self%arr, val%arr) == 1) then
                ret%arr = sub(self%arr, val%arr)
                ret%sgn = '-'
            else
                ret%arr = sub(val%arr, self%arr)
                ret%sgn = '+'
            end if
        end if
    end function sub_func

    !> Substract a `multiprecision_t` by an integer.
    pure function sub_int_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%sub_func(to_long(val))
    end function sub_int_func

    !> Substract a `multiprecision_t` by an character.
    pure function sub_char_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%sub_func(to_long(val))
    end function sub_char_func

    !> Multiply a `multiprecision_t` by a `multiprecision_t`.
    pure function mul_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val
        type(multiprecision_t) :: ret

        if (self == 0 .or. val == 0) then
            ret%sgn = '+'
            ret%arr = [0]
        else if (self%sgn == val%sgn) then
            ret%sgn = '+'
            ret%arr = mul(self%arr, val%arr)
        else if (self%sgn /= val%sgn) then
            ret%sgn = '-'
            ret%arr = mul(self%arr, val%arr)
        end if
    end function mul_func

    !> Multiply a `multiprecision_t` by an integer.
    pure function mul_int_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%mul_func(to_long(val))
    end function mul_int_func

    !> Multiply a `multiprecision_t` by a character.
    pure function mul_char_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%mul_func(to_long(val))
    end function mul_char_func

    !> Power of an integer.
    pure function pow_int_func(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val
        type(multiprecision_t) :: ret

        if (self%sgn == '+') then
            ret%sgn = '+'
            ret%arr = pow2(self%arr, val)
        else
            if (mod(val, 2) == 0) then
                ret%sgn = '+'
            else if (mod(val, 2) /= 0) then
                ret%sgn = '-'
            end if
            ret%arr = pow2(self%arr, val)
        end if
    end function pow_int_func

end module multiprecision_m

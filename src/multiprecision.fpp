#: set names = ['char', 'i32', 'i32_arr']
#: set types = ['character(*)', 'integer(i32)', 'integer(i32), dimension(:)']
#: set names_types = list(zip(names, types))
#: set operators = ['==', '>', '<', '>=', '<=', '+', '-', '*']
#: set funcs = ['eq', 'gt', 'lt', 'ge', 'le', 'add', 'sub', 'mul']
#: set operators_funcs = list(zip(operators, funcs))

module big_integer_m

    use constant_m, only: i32
    implicit none
    private

    public :: big_
    public :: big_integer
    public :: len, swap
    public :: assignment(=)
    #: for o in operators
    public :: operator(${o}$)
    #: endfor
    public :: operator(**)

    !> Type big integer
    type :: big_integer
        integer(i32), allocatable :: arr(:)
        logical :: sgn
    end type big_integer

    !> Assignment(=)
    interface assignment(=)
        module procedure init_char, init_i32, init_i32_arr
    end interface assignment(=)

    !> Convert to big_integer
    interface big_
        #: for name in names
        module procedure big_${name}$
        #: endfor
    end interface big_

    interface len
        module procedure len_big
    end interface len

    interface swap
        module procedure swap_big
    end interface swap

    #: for o, f in operators_funcs
    !> Operator(${o}$)
    interface operator(${o}$)
        module procedure ${f}$#{for n in names}#, ${f}$_${n}$#{endfor}#
    end interface operator(${o}$)

    #: endfor
    !> Operator(**)
    interface operator(**)
        module procedure pow_i32
    end interface operator(**)

contains

    !> The core function to cut leading zeros of a contiguous array.
    pure function cut_leading_zeros(arr) result(ret)
        integer(i32), contiguous, intent(in) :: arr(:)
        integer(i32), allocatable :: ret(:)

        ret = arr(findloc(arr == 0, value=.false., dim=1):)
    end function cut_leading_zeros

    !> The core function to carry digits during add_ and multiplication.
    pure function carry(arr) result(ret)
        integer(i32), contiguous, intent(in) :: arr(:)
        integer(i32), dimension(size(arr) + 2) :: tmp1, tmp2, ret

        tmp1 = 0; tmp2 = 0; ret(1:2) = 0; ret(3:) = arr(:)
        do
            if (all(ret <= 9)) exit
            tmp1(:) = ret(:) - ret(:)/10*10
            tmp2(1:size(tmp2) - 1) = ret(2:)/10
            ret(:) = tmp1(:) + tmp2(:)
        end do
    end function carry

    !> The core function to provide the ability of addition.
    pure function add_(arr1, arr2) result(ret)
        integer(i32), contiguous, intent(in) :: arr1(:), arr2(:)
        integer(i32), allocatable, dimension(:) :: ret, tmp1, tmp2

        associate (x => max(size(arr1), size(arr2)) + 1)
            allocate (tmp1(x), tmp2(x))
            tmp1 = 0; tmp1(x - size(arr1) + 1:x) = arr1(:)
            tmp2 = 0; tmp2(x - size(arr2) + 1:x) = arr2(:)
        end associate
        ret = cut_leading_zeros(carry(tmp1 + tmp2))
    end function add_

    !> The core function to provide the ability of subtraction.
    pure function sub_(arr1, arr2) result(ret)
        integer(i32), contiguous, intent(in) :: arr1(:), arr2(:)
        integer(i32), allocatable, dimension(:) :: ret, tmp1, tmp2, tmp
        integer(i32) :: i

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
    end function sub_

    !> The core function to provide the ability of comparison.
    pure function comp_(arr1, arr2) result(ret)
        integer(i32), contiguous, intent(in) :: arr1(:), arr2(:)
        integer(i32) :: ret, i

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
        if (ret /= 0) error stop 'comp_: invalid output.'
    end function comp_

    !> The core function to provide the ability of multiplication.
    pure function mul_(arr1, arr2) result(ret)
        integer(i32), contiguous, intent(in) :: arr1(:), arr2(:)
        integer(i32), allocatable :: tmp(:), tmp_row(:), ret(:)
        integer(i32) :: i

        associate (s1 => size(arr1), s2 => size(arr2))
            allocate (tmp_row(s1 + s2), tmp(s1 + s2 + 2))
            tmp = 0

            do i = s2, 1, -1
                tmp_row = 0; tmp_row(i + 1:i + s1) = arr1
                tmp = tmp + carry(tmp_row*arr2(i))
            end do
        end associate
        ret = cut_leading_zeros(carry(tmp))
    end function mul_

    !> The core function to provide the ability of power.
    pure function pow_(arr, p) result(ret)
        integer(i32), contiguous, intent(in) :: arr(:)
        integer(i32), intent(in) :: p
        integer(i32), allocatable :: ret(:)
        integer(i32) :: n
        integer(i32), allocatable :: x(:), y(:)

        n = p
        if (n < 0) then
            error stop 'pow_: n is nonnegative.'
        else if (n == 1) then
            ret = [1]
        else
            x = arr; y = [1]
            do while (n > 1)
                if (mod(n, 2) == 0) then
                    x = mul_(x, x)
                    n = n/2
                else
                    y = mul_(x, y)
                    x = mul_(x, x)
                    n = (n - 1)/2
                end if
            end do
        end if
        ret = mul_(x, y)
    end function pow_

    !> Re-allocate a big_integer with length n.
    pure subroutine re_alloc(value_, n)
        type(big_integer), intent(inout) :: value_
        integer(i32), intent(in) :: n

        if (allocated(value_%arr)) then
            if (size(value_%arr) /= n) then
                deallocate (value_%arr)
                allocate (value_%arr(n))
            end if
        else
            allocate (value_%arr(n))
        end if
    end subroutine re_alloc

    !> Initialize a big_integer with an integer.
    pure subroutine init_i32(value_, int_)
        type(big_integer), intent(inout) :: value_
        integer(i32), intent(in) :: int_
        integer(i32) :: temp, i

        value_%sgn = int_ >= 0
        if (int_ == 0) then
            value_%arr = [0]; return
        end if

        temp = abs(int_)
        associate (digits => floor(log10(real(temp))) + 1)
            call re_alloc(value_, digits)
            do i = digits, 1, -1
                value_%arr(i) = int(mod(temp, 10))
                temp = temp/10
            end do
        end associate
    end subroutine init_i32

    !> Initialize a big_integer with a character.
    pure subroutine init_char(value_, char_)
        type(big_integer), intent(inout) :: value_
        character(len=*), intent(in) :: char_

        select case (char_(1:1))
        case ('+', '-')
            value_%sgn = char_(1:1) == '+'
            call re_alloc(value_, len(char_) - 1)
            read (char_(2:len(char_)), "(*(i1))") value_%arr
        case default
            value_%sgn = .true.
            call re_alloc(value_, len(char_))
            read (char_(1:len(char_)), "(*(i1))") value_%arr
        end select
    end subroutine init_char

    !> Initialize a big_integer with 
    pure subroutine init_i32_arr(value_, arr_)
        type(big_integer), intent(inout) :: value_
        integer(i32), intent(in) :: arr_(:)

        call re_alloc(value_, size(arr_))
        value_%arr = arr_(:)
        value_%sgn = .true.
    end subroutine init_i32_arr

    #: for name, T in names_types
    !> Convert ${name}$ to big_integer
    pure type(big_integer) function big_${name}$ (val)
        ${T}$, intent(in) :: val

        big_${name}$ = val
    end function big_${name}$

    #: endfor

    !> Number of digits of a big integer.
    pure integer(i32) function len_big(val)
        type(big_integer), intent(in) :: val

        len_big = size(val%arr)
    end function len_big

    !> Swap
    pure subroutine swap_big(a, b)
        type(big_integer), intent(inout) :: a, b
        type(big_integer) :: temp

        temp = a; a = b; b = temp
    end subroutine swap_big

    !> Equal.
    pure logical function eq(value_1, value_2)
        type(big_integer), intent(in) :: value_1, value_2

        eq = .false.
        if (comp_(value_1%arr, value_2%arr) == 0 .and. &
            (value_1%sgn .eqv. value_2%sgn)) eq = .true.
    end function eq

    !> Greater than.
    pure logical function gt(value_1, value_2)
        type(big_integer), intent(in) :: value_1, value_2
        logical :: condition_satisfied(3), all_positive, all_negative

        associate (c => (comp_(value_1%arr, value_2%arr)))
            all_positive = all([value_1%sgn, value_2%sgn])
            all_negative = all([.not. value_1%sgn,.not. value_2%sgn])
            condition_satisfied = &
                [value_1%sgn .and. .not. value_2%sgn, &
                 c == 1 .and. all_positive, c == -1 .and. all_negative]
        end associate

        if (any(condition_satisfied)) then
            gt = .true.
        else
            gt = .false.
        end if
    end function gt

    !> Less than.
    pure logical function lt(value_1, value_2)
        type(big_integer), intent(in) :: value_1, value_2
        logical :: condition_satisfied(3), all_positive, all_negative

        associate (c => (comp_(value_1%arr, value_2%arr)))
            all_positive = all([value_1%sgn, value_2%sgn])
            all_negative = all([.not. value_1%sgn,.not. value_2%sgn])
            condition_satisfied = &
                [.not. value_1%sgn .and. value_2%sgn, &
                 c == -1 .and. all_positive, c == 1 .and. all_negative]
        end associate

        if (any(condition_satisfied)) then
            lt = .true.
        else
            lt = .false.
        end if
    end function lt

    !> Greater than or equal to.
    pure logical function ge(value_1, value_2)
        type(big_integer), intent(in) :: value_1, value_2

        ge = .false.
        if (gt(value_1, value_2) .or. eq(value_1, value_2)) ge = .true.
    end function ge

    !> Less than or equal to.
    pure logical function le(value_1, value_2)
        type(big_integer), intent(in) :: value_1, value_2

        le = .false.
        if (lt(value_1, value_2) .or. eq(value_1, value_2)) le = .true.
    end function le

    !> Addition.
    pure function add(value_1, value_2) result(ret)
        type(big_integer), intent(in) :: value_1, value_2
        type(big_integer) :: ret

        if (comp_(value_1%arr, value_2%arr) == 0 .and. &
            .not. (value_1%sgn .and. value_2%sgn)) then
            ret = 0
        else if (all([value_1%sgn, value_2%sgn])) then
            ret%arr = add_(value_1%arr, value_2%arr)
            ret%sgn = .true.
        else if (.not. value_1%sgn .and. .not. value_2%sgn) then
            ret%arr = add_(value_1%arr, value_2%arr)
            ret%sgn = .false.
        else if (value_1%sgn .and. .not. value_2%sgn) then
            if (comp_(value_1%arr, value_2%arr) == 1) then
                ret%arr = sub_(value_1%arr, value_2%arr)
                ret%sgn = .true.
            else
                ret%arr = sub_(value_1%arr, value_2%arr)
                ret%sgn = .false.
            end if
        else if (.not. value_1%sgn .and. value_2%sgn) then
            if (comp_(value_1%arr, value_2%arr) == 1) then
                ret%arr = sub_(value_1%arr, value_2%arr)
                ret%sgn = .false.
            else
                ret%arr = sub_(value_1%arr, value_2%arr)
                ret%sgn = .true.
            end if
        end if
    end function add

    !> Subtraction.
    pure function sub(value_1, value_2) result(ret)
        type(big_integer), intent(in) :: value_1, value_2
        type(big_integer) :: ret

        if (value_1 == value_2) then
            ret%arr = [0]
            ret%sgn = .true.
        else if (value_1%sgn .and. value_2%sgn) then
            if (comp_(value_1%arr, value_2%arr) == 1) then
                ret%arr = sub_(value_1%arr, value_2%arr)
                ret%sgn = .true.
            else
                ret%arr = sub_(value_2%arr, value_1%arr)
                ret%sgn = .false.
            end if
        else if (value_1%sgn .and. .not. value_2%sgn) then
            ret%arr = add_(value_1%arr, value_2%arr)
            ret%sgn = .true.
        else if (.not. value_1%sgn .and. value_2%sgn) then
            ret%arr = add_(value_1%arr, value_2%arr)
            ret%sgn = .false.
        else if (.not. value_1%sgn .and. .not. value_2%sgn) then
            if (comp_(value_1%arr, value_2%arr) == 1) then
                ret%arr = sub_(value_1%arr, value_2%arr)
                ret%sgn = .false.
            else
                ret%arr = sub_(value_2%arr, value_1%arr)
                ret%sgn = .true.
            end if
        end if
    end function sub

    !> Multiplication.
    pure function mul(value_1, value_2) result(ret)
        type(big_integer), intent(in) :: value_1, value_2
        type(big_integer) :: ret

        if (value_1 == big_(0) .or. value_2 == big_(0)) then
            ret%sgn = .true.
            ret%arr = [0]
        else if (value_1%sgn .eqv. value_2%sgn) then
            ret%sgn = .true.
            ret%arr = mul_(value_1%arr, value_2%arr)
        else if (value_1%sgn .neqv. value_2%sgn) then
            ret%sgn = .false.
            ret%arr = mul_(value_1%arr, value_2%arr)
        end if
    end function mul

    #: for f in ['eq', 'gt', 'lt', 'ge', 'le']
    #: for name, T in names_types
    !> Wrapper of function '${f}$' for type ${T}$.
    pure function ${f}$_${name}$ (value_1, value_2) result(ret)
        type(big_integer), intent(in) :: value_1
        ${T}$, intent(in) :: value_2
        logical :: ret

        ret = ${f}$ (value_1, big_(value_2))
    end function ${f}$_${name}$

    #: endfor
    #: endfor

    #: for f in ['add', 'sub', 'mul']
    #: for name, T in names_types
    !> Wrapper of function '${f}$' for type ${T}$.
    pure function ${f}$_${name}$ (value_1, value_2) result(ret)
        type(big_integer), intent(in) :: value_1
        ${T}$, intent(in) :: value_2
        type(big_integer) :: ret

        ret = ${f}$ (value_1, big_(value_2))
    end function ${f}$_${name}$

    #: endfor
    #: endfor

    !> Power.
    pure function pow_i32(value_1, value_2) result(ret)
        type(big_integer), intent(in) :: value_1
        integer(i32), intent(in) :: value_2
        type(big_integer) :: ret

        ret%sgn = .true.
        if (.not. value_1%sgn .and. mod(value_2, 2) /= 0) ret%sgn = .false.
        ret%arr = pow_(value_1%arr, value_2)
    end function pow_i32

end module big_integer_m

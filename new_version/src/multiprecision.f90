module multiprecision_utility_m

    use constant_m, only: i32
    implicit none
    private
    public :: add_, sub_, comp_, mul_, pow_

contains

    !> The core function to cut leading zeros of a contiguous array.
    !>
    !> As the name of the function suggests, the function
    !> deletes all the leading zeros of a contiguous array.
    !> This procedure uses the intrinsic function `findloc`
    !> that is only available with gfortran > 9.0.0
    pure function cut_leading_zeros(arr) result(ret)
        integer(i32), contiguous, intent(in) :: arr(:)
        integer(i32), allocatable :: ret(:)

        ret = arr(findloc(arr == 0, value=.false., dim=1):)
    end function cut_leading_zeros

    !> The core function to carry digits during add_ and multiplication.
    !>
    !> Each element of the input array is an 32bit integer greater than or
    !> equal to zero and less than or equal to nine.
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
    !>
    !> Each element of the input array is an 32bit integer greater than or
    !> equal to zero and less than or equal to nine. When two such arrays
    !> are "added", first of all, the shorter array will be extended to the
    !> length of the longer array and two arrays with the same dimension
    !> are added. Then the function `carry`
    !> "[carries](https://en.wikipedia.org/wiki/Carry_(arithmetic))" the
    !> added array iteratively until all elements of the array are greater
    !> than or equal to zero and less than or equal to nine. Finally, the
    !> leading zeros are deleted by `cut_leading_zeros`.
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
    !>
    !> Each element of the input array is an 32bit integer greater than or
    !> equal to zero and less than or equal to nine.
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
    !>
    !>### Result
    !> | return value | result |
    !> |:------------:|:------:|
    !> |`ret = 1`  |arr1 > arr2|
    !> |`ret = 0`  |arr1 = arr2|
    !> |`ret = -1` |arr1 < arr2|
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
    !>
    !> Each element of the input array is an 32bit integer greater than or
    !> equal to zero and less than or equal to nine.
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
    !>
    !> Each element of the input array is an 32bit integer greater than or
    !> equal to zero and less than or equal to nine. The function uses
    !> a very simple implementation of exponentiation
    !> ([wiki links](https://en.wikipedia.org/wiki/
    !>Exponentiation_by_squaring)). The function currently doesn't
    !> support negative powers.
    pure recursive function pow_(arr, n) result(ret)
        integer(i32), contiguous, intent(in) :: arr(:)
        integer(i32), intent(in) :: n
        integer(i32), allocatable :: ret(:)

        if (n < 0) error stop 'pow_: n is nonnegative.'

        if (n == 0) then
            ret = [1]
        else if (n == 1) then
            ret = arr
        else if (mod(n, 2) == 0) then
            ret = pow_(mul_(arr, arr), n/2)
        else if (mod(n, 2) /= 0) then
            ret = pow_(mul_(arr, arr), (n - 1)/2)
            ret = mul_(arr, ret)
        end if
    end function pow_

end module multiprecision_utility_m

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
        procedure, private :: re_alloc
        procedure, private :: init, init_char, init_int, init_arr
        generic :: assignment(=) => init, init_char, init_int, init_arr
        procedure, private :: eq, eq_int, eq_char
        generic :: operator(==) => eq, eq_int, eq_char
        procedure, private :: gt, gt_int, gt_char
        generic :: operator(>) => gt, gt_int, gt_char
        procedure, private :: lt, lt_int, lt_char
        generic :: operator(<) => lt, lt_int, lt_char
        procedure, private :: ge, ge_int, ge_char
        generic :: operator(>=) => ge, ge_int, ge_char
        procedure, private :: le, le_int, le_char
        generic :: operator(<=) => le, le_int, le_char
        procedure, private :: add, add_int, add_char
        generic :: operator(+) => add, add_int, add_char
        procedure, private :: sub, sub_int, sub_char
        generic :: operator(-) => sub, sub_int, sub_char
        procedure, private :: mul, mul_int, mul_char
        generic :: operator(*) => mul, mul_int, mul_char
        ! procedure, private :: div, div_int, div_char
        ! generic :: operator(/) => div, div_int, div_char
        procedure, private :: pow_int
        generic :: operator(**) => pow_int
        ! procedure, private :: fac, fac_int, fac_char
        ! generic :: operator(.fac.) => fac, fac_int, fac_char
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
        module procedure to_long_char, to_long_int, to_long_arr
    end interface

contains

    !> Reallocate a `multiprecision_t`.
    !> This subroutine prevents unnecessary deallocations and re-allocations
    !> of `self%arr`. This is particularly useful when `=` appears in a loop.
    pure subroutine re_alloc(self, n)
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
    end subroutine re_alloc

    !> Initialize a `multiprecision_t` to a `multiprecision_t`
    pure subroutine init(self, val)
        class(multiprecision_t), intent(inout) :: self
        type(multiprecision_t), intent(in) :: val

        call self%re_alloc(size(val%arr))
        self%sgn = val%sgn
        self%arr = val%arr
    end subroutine init

    !> Initialize a character type to a `multiprecision_t`.
    pure subroutine init_char(self, chars)
        class(multiprecision_t), intent(inout) :: self
        character(len=*), intent(in) :: chars
        integer(i32) :: i

        select case (chars(1:1))
        case ('+', '-')
            self%sgn = chars(1:1)
            call self%re_alloc(len(chars) - 1)
            read (chars(2:len(chars)), "(*(i1))") self%arr
        case default
            self%sgn = '+'
            call self%re_alloc(len(chars))
            read (chars(1:len(chars)), "(*(i1))") self%arr
        end select
    end subroutine init_char

    !> Initialize an integer array to a `multiprecision_t`.
    pure subroutine init_arr(self, arr)
        class(multiprecision_t), intent(inout) :: self
        integer(i32), intent(in) :: arr(:)

        call self%re_alloc(size(arr))
        self%arr = arr(:)
        self%sgn = '+'
    end subroutine init_arr

    !> Initialize an integer to a `multiprecision_t`.
    pure subroutine init_int(self, val)
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
    end subroutine init_int

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

    !> Convert an integer array to a `multiprecision_t`.
    pure function to_long_arr(val) result(ret)
        integer(i32), intent(in) :: val(:)
        type(multiprecision_t) :: ret

        ret = val
    end function to_long_arr

    !> To judge whether two `multiprecision_t`s are equal.
    pure logical function eq(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        eq = .false.
        if (comp_(self%arr, val%arr) == 0 .and. &
            self%sgn == val%sgn) eq = .true.
    end function eq

    !> To judge whether a `multiprecision_t` and an integer are equal.
    pure logical function eq_int(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        eq_int = self%eq(to_long(val))
    end function eq_int

    !> To judge whether a `multiprecision_t` and an character are equal.
    pure logical function eq_char(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        eq_char = self%eq(to_long(val))
    end function eq_char

    !> To judge whether a `multiprecision_t` is
    !> greater than a `multiprecision_t`.
    pure logical function gt(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        gt = .false.
        if (self%sgn == '+' .and. val%sgn == '-') then
            gt = .true.
        else if (comp_(self%arr, val%arr) == 1 .and. &
                 self%sgn == '+' .and. val%sgn == '+') then
            gt = .true.
        else if (comp_(self%arr, val%arr) == -1 .and. &
                 self%sgn == '-' .and. val%sgn == '-') then
            gt = .true.
        end if
    end function gt

    !> To judge whether a `multiprecision_t` is
    !> greater than an integer.
    pure logical function gt_int(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        gt_int = self%gt(to_long(val))
    end function gt_int

    !> To judge whether a `multiprecision_t` is
    !> greather than an character.
    pure logical function gt_char(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        gt_char = self%gt(to_long(val))
    end function gt_char

    !> To judge whether a `multiprecision_t` is
    !> less than a `multiprecision_t`.
    pure logical function lt(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        lt = .false.
        if (self%sgn == '-' .and. val%sgn == '+') then
            lt = .true.
        else if (comp_(self%arr, val%arr) == -1 .and. &
                 self%sgn == '+' .and. val%sgn == '+') then
            lt = .true.
        else if (comp_(self%arr, val%arr) == 1 .and. &
                 self%sgn == '-' .and. val%sgn == '-') then
            lt = .true.
        end if
    end function lt

    !> To judge whether a `multiprecision_t` is
    !> less than an integer.
    pure logical function lt_int(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        lt_int = self%lt(to_long(val))
    end function lt_int

    !> To judge whether a `multiprecision_t` is
    !> less than a character.
    pure logical function lt_char(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        lt_char = self%lt(to_long(val))
    end function lt_char

    !> To judge whether a `multiprecision_t` is
    !> greater than or equal to a `multiprecision_t`.
    pure logical function ge(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        ge = .false.
        if (self%gt(val) .or. self%eq(val)) ge = .true.
    end function ge

    !> To judge whether a `multiprecision_t` is
    !> greater than or equal to a `multiprecision_t`.
    pure logical function ge_int(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        ge_int = self%ge(to_long(val))
    end function ge_int

    !> To judge whether a `multiprecision_t` is
    !> greater than or equal to a character.
    pure logical function ge_char(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        ge_char = self%ge(to_long(val))
    end function ge_char

    !> To judge whether a `multiprecision_t` is
    !> less than or equal to a `multiprecision_t`.
    pure logical function le(self, val)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val

        le = .false.
        if (self%lt(val) .or. self%eq(val)) le = .true.
    end function le

    !> To judge whether a `multiprecision_t` is
    !> less than or equal to a integer.
    pure logical function le_int(self, val)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val

        le_int = self%le(to_long(val))
    end function le_int

    !> To judge whether a `multiprecision_t` is
    !> less than or equal to a character.
    pure logical function le_char(self, val)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val

        le_char = self%le(to_long(val))
    end function le_char

    !> Add two `multiprecision_t`s.
    pure function add(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val
        type(multiprecision_t) :: ret

        if (comp_(self%arr, val%arr) == 0 .and. &
            self%sgn /= val%sgn) then
            ret = '0'
        else if (self%sgn == '+' .and. val%sgn == '+') then
            ret%arr = add_(self%arr, val%arr)
            ret%sgn = '+'
        else if (self%sgn == '-' .and. val%sgn == '-') then
            ret%arr = add_(self%arr, val%arr)
            ret%sgn = '-'
        else if (self%sgn == '+' .and. val%sgn == '-') then
            if (comp_(self%arr, val%arr) == 1) then
                ret%arr = sub_(self%arr, val%arr)
                ret%sgn = '+'
            else
                ret%arr = sub_(val%arr, self%arr)
                ret%sgn = '-'
            end if
        else if (self%sgn == '-' .and. val%sgn == '+') then
            if (comp_(self%arr, val%arr) == 1) then
                ret%arr = sub_(self%arr, val%arr)
                ret%sgn = '-'
            else
                ret%arr = sub_(val%arr, self%arr)
                ret%sgn = '+'
            end if
        end if
    end function

    !> Add a `multiprecision_t` and an integer.
    pure function add_int(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%add(to_long(val))
    end function add_int

    !> Add a `multiprecision_t` and a character.
    pure function add_char(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%add(to_long(val))
    end function add_char

    !> Substract a `multiprecision_t` by a `multiprecision_t`.
    pure function sub(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val
        type(multiprecision_t) :: ret

        if (self == val) then
            ret = '0'
        else if (self%sgn == '+' .and. val%sgn == '+') then
            if (comp_(self%arr, val%arr) == 1) then
                ret%arr = sub_(self%arr, val%arr)
                ret%sgn = '+'
            else
                ret%arr = sub_(val%arr, self%arr)
                ret%sgn = '-'
            end if
        else if (self%sgn == '+' .and. val%sgn == '-') then
            ret%arr = add_(self%arr, val%arr)
            ret%sgn = '+'
        else if (self%sgn == '-' .and. val%sgn == '+') then
            ret%arr = add_(self%arr, val%arr)
            ret%sgn = '-'
        else if (self%sgn == '-' .and. val%sgn == '-') then
            if (comp_(self%arr, val%arr) == 1) then
                ret%arr = sub_(self%arr, val%arr)
                ret%sgn = '-'
            else
                ret%arr = sub_(val%arr, self%arr)
                ret%sgn = '+'
            end if
        end if
    end function sub

    !> Substract a `multiprecision_t` by an integer.
    pure function sub_int(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%sub(to_long(val))
    end function sub_int

    !> Substract a `multiprecision_t` by an character.
    pure function sub_char(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%sub(to_long(val))
    end function sub_char

    !> Multiply a `multiprecision_t` by a `multiprecision_t`.
    pure function mul(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        type(multiprecision_t), intent(in) :: val
        type(multiprecision_t) :: ret

        if (self == 0 .or. val == 0) then
            ret%sgn = '+'
            ret%arr = [0]
        else if (self%sgn == val%sgn) then
            ret%sgn = '+'
            ret%arr = mul_(self%arr, val%arr)
        else if (self%sgn /= val%sgn) then
            ret%sgn = '-'
            ret%arr = mul_(self%arr, val%arr)
        end if
    end function mul

    !> Multiply a `multiprecision_t` by an integer.
    pure function mul_int(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%mul(to_long(val))
    end function mul_int

    !> Multiply a `multiprecision_t` by a character.
    pure function mul_char(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        character(len=*), intent(in) :: val
        type(multiprecision_t) :: ret

        ret = self%mul(to_long(val))
    end function mul_char

    !> Power of an integer.
    pure function pow_int(self, val) result(ret)
        class(multiprecision_t), intent(in) :: self
        integer(i32), intent(in) :: val
        type(multiprecision_t) :: ret

        if (self%sgn == '+') then
            ret%sgn = '+'
            ret%arr = pow_(self%arr, val)
        else
            if (mod(val, 2) == 0) then
                ret%sgn = '+'
            else if (mod(val, 2) /= 0) then
                ret%sgn = '-'
            end if
            ret%arr = pow_(self%arr, val)
        end if
    end function pow_int

end module multiprecision_m

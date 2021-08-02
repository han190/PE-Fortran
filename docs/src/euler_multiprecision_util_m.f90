module euler_multiprecision_util_m

    implicit none
    private
    public :: add, sub, compare, mul, pow2

contains

    !> The core function to cut leading zeros of a contiguous array.
    !>
    !> As the name of the function suggests, the function
    !> deletes all the leading zeros of a contiguous array.
    !> This procedure uses the intrinsic function `findloc`
    !> that is only available with gfortran > 9.0.0
    pure function cut_leading_zeros(arr) result(ret)
        integer, contiguous, intent(in) :: arr(:)
        integer, allocatable :: ret(:)

        ret = arr(findloc(arr == 0, value=.false., dim=1):)
    end function cut_leading_zeros

    !> The core function to carry digits during add and multiplication.
    !>
    !> Each element of the input array is an 32bit integer greater than or
    !> equal to zero and less than or equal to nine.
    pure function carry(arr) result(ret)
        integer, contiguous, intent(in) :: arr(:)
        integer, dimension(size(arr) + 2) :: tmp1, tmp2, ret

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
    pure function add(arr1, arr2) result(ret)
        integer, contiguous, intent(in) :: arr1(:), arr2(:)
        integer, allocatable, dimension(:) :: ret, tmp1, tmp2

        associate (x => max(size(arr1), size(arr2)) + 1)
            allocate (tmp1(x), tmp2(x))
            tmp1 = 0; tmp1(x - size(arr1) + 1:x) = arr1(:)
            tmp2 = 0; tmp2(x - size(arr2) + 1:x) = arr2(:)
        end associate
        ret = cut_leading_zeros(carry(tmp1 + tmp2))
    end function add

    !> The core function to provide the ability of subtraction.
    !>
    !> Each element of the input array is an 32bit integer greater than or
    !> equal to zero and less than or equal to nine.
    pure function sub(arr1, arr2) result(ret)
        integer, contiguous, intent(in) :: arr1(:), arr2(:)
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

    !> The core function to provide the ability of comparison.
    !>
    !>### Result
    !> | return value | result |
    !> |:------------:|:------:|
    !> |`ret = 1`  |arr1 > arr2|
    !> |`ret = 0`  |arr1 = arr2|
    !> |`ret = -1` |arr1 < arr2|
    pure function compare(arr1, arr2) result(ret)
        integer, contiguous, intent(in) :: arr1(:), arr2(:)
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

    !> The core function to provide the ability of multiplication.
    !>
    !> Each element of the input array is an 32bit integer greater than or
    !> equal to zero and less than or equal to nine.
    pure function mul(arr1, arr2) result(ret)
        integer, contiguous, intent(in) :: arr1(:), arr2(:)
        integer, allocatable :: tmp(:), tmp_row(:), ret(:)
        integer :: i

        associate (s1 => size(arr1), s2 => size(arr2))
            allocate (tmp_row(s1 + s2), tmp(s1 + s2 + 2))
            tmp = 0

            do i = s2, 1, -1
                tmp_row = 0; tmp_row(i + 1:i + s1) = arr1
                tmp = tmp + carry(tmp_row*arr2(i))
            end do
        end associate
        ret = cut_leading_zeros(carry(tmp))
    end function mul

    !> The core function to provide the ability of power.
    !>
    !> Each element of the input array is an 32bit integer greater than or
    !> equal to zero and less than or equal to nine. The function uses
    !> a very simple implementation of exponentiation
    !> ([wiki links](https://en.wikipedia.org/wiki/
    !>Exponentiation_by_squaring)). The function currently doesn't
    !> support negative powers.
    pure recursive function pow2(arr, n) result(ret)
        integer, contiguous, intent(in) :: arr(:)
        integer, intent(in) :: n
        integer, allocatable :: ret(:)

        if (n < 0) error stop 'pow2: n is nonnegative.'

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

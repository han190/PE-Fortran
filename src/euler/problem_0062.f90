submodule(interface_m) euler_problem_0062_m
    implicit none

contains

    module character(len=20) function euler0062()
        write (euler0062, "(i20)") answer()
    end function euler0062

    pure integer(i64) function answer()
        use big_integer_m
        implicit none

        integer(i64) :: i, x
        integer(i64), allocatable :: count_(:)
        character(len=20), allocatable :: list(:)
        integer(i64), parameter :: upper_ = 10000

        allocate (list(upper_), count_(upper_))
        list = ""; count_ = 1
        do i = 1, upper_
            write (list(i), "(*(i1))") sort(to_array(i**3))
            x = findloc(list(:i - 1), value=list(i), dim=1)

            if (x /= 0) then
                count_(x) = count_(x) + 1
                if (count_(x) == 5) exit
            end if
        end do
        answer = x**3
    end function answer

    pure function sort(arr) result(ret)
        integer(i64), intent(in) :: arr(:)
        integer(i64), allocatable :: ret(:)

        ret = arr
        call quick_sort(ret, 1_i64, size(ret, kind=i64))
    end function sort

    pure recursive subroutine quick_sort(arr, low, high)
        integer(i64), intent(inout) :: arr(:)
        integer(i64), intent(in) :: low, high
        integer(i64) :: p

        if (low < high) then
            call partition(arr, p, low, high)
            call quick_sort(arr, low, p - 1)
            call quick_sort(arr, p + 1, high)
        end if
    end subroutine quick_sort

    pure subroutine partition(arr, i, low, high)
        integer(i64), intent(inout) :: arr(:), i
        integer(i64), intent(in) :: low, high
        integer(i64) :: j, pivot

        pivot = arr(high)
        i = low
        do j = low, high
            if (arr(j) < pivot) then
                call swap(arr(i), arr(j))
                i = i + 1
            end if
        end do
        call swap(arr(i), arr(high))
    end subroutine partition

end submodule euler_problem_0062_m
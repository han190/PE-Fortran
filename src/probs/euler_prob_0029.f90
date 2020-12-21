submodule(euler_interface_m) euler_prob_0029_m
    implicit none

contains

    module character(len=20) function euler0029()
        write (euler0029, "(i20)") ans(99)
    end function euler0029

    integer function ans(n)
        integer, intent(in) :: n
        integer, dimension(2, n, n) :: arr_of_nums
        integer, dimension(2, n*n) :: res, arr
        integer :: i, j, k

        do i = 2, n + 1
            do j = 2, n + 1
                arr_of_nums(1, j - 1, i - 1) = j
                arr_of_nums(2, j - 1, i - 1) = i
                call simplify_powers( &
                    arr_of_nums(:, j - 1, i - 1) &
                    )
            end do
        end do

        call reshape_to_2_by_99_x_99(arr_of_nums, arr)
        res(1:2, 1) = arr(1:2, 1); k = 1

        outer: do i = 2, n*n
            inner: do j = 1, k
                if ( &
                    res(1, j) == arr(1, i) .and. &
                    res(2, j) == arr(2, i) &
                    ) cycle outer
            end do inner
            k = k + 1
            res(1:2, k) = arr(1:2, i)
        end do outer

        ans = k
    end function ans

    subroutine simplify_powers(arr1)
        integer, intent(inout) :: arr1(2)

        select case (arr1(1))
        case (4, 9, 25, 36, 49, 100)
            arr1(1) = int(sqrt(real(arr1(1), sp)))
            arr1(2) = arr1(2)*2
        case (8)
            arr1(1) = 2
            arr1(2) = arr1(2)*3
        case (27)
            arr1(1) = 3
            arr1(2) = arr1(2)*3
        case (16)
            arr1(1) = 2
            arr1(2) = arr1(2)*4
        case (81)
            arr1(1) = 3
            arr1(2) = arr1(2)*4
        case (32)
            arr1(1) = 2
            arr1(2) = arr1(2)*5
        case (64)
            arr1(1) = 2
            arr1(2) = arr1(2)*6
        case default
            arr1(1:2) = arr1(1:2)
        end select
    end subroutine simplify_powers

    subroutine reshape_to_2_by_99_x_99(arr_in, arr_out)
        integer, intent(in) :: arr_in(:, :, :)
        integer, intent(out) :: arr_out(:, :)

        arr_out = reshape(arr_in, [2, 99*99])
    end subroutine reshape_to_2_by_99_x_99

end submodule euler_prob_0029_m

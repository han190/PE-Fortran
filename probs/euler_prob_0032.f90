submodule(euler_interface_m) euler_prob_0032_m
    implicit none

contains

    module character(len=20) function euler0032()
        write (euler0032, "(i20)") ans()
    end function euler0032

    integer(int64) function ans()
        integer(int64) :: temp, i, j, k, arr_of_prods(9)
        integer(int64), allocatable :: arr_of_nonrepeats(:)

        k = 1_int64
        do i = 1_int64, 9_int64
            do j = 1234_int64, 9876_int64
                if ( &
                    digs_of_int(i*j) > 4_int64 &
                    ) cycle
                temp = i*10_int64**8 + j*10_int64**4 + i*j

                if ( &
                    is_pandigital(temp, digs_of_int(temp)) &
                    ) then
                    arr_of_prods(k) = i*j
                    k = k + 1_int64
                end if
            end do
        end do

        do i = 12_int64, 98_int64
            do j = 123_int64, 987_int64
                if ( &
                    digs_of_int(i*j) > 4_int64 &
                    ) cycle
                temp = i*10_int64**7 + j*10_int64**4 + i*j
                if ( &
                    is_pandigital(temp, digs_of_int(temp)) &
                    ) then
                    arr_of_prods(k) = i*j
                    k = k + 1_int64
                end if
            end do
        end do

        call remove_duplicates(arr_of_prods, arr_of_nonrepeats)
        ans = sum(arr_of_nonrepeats, dim=1)
    end function ans

    subroutine remove_duplicates(i_arr, o_arr)
        integer(int64), intent(in) :: i_arr(:)
        integer(int64), allocatable, intent(out) :: o_arr(:)
        integer(int64) :: i, k, tmp_arr(size(i_arr))

        tmp_arr = 0_int64
        k = 1_int64
        tmp_arr(1) = i_arr(1)

        do i = 2_int64, size(i_arr)
            if (any(tmp_arr == i_arr(i))) cycle
            k = k + 1_int64
            tmp_arr(k) = i_arr(i)
        end do

        allocate (o_arr(k))
        o_arr(:) = tmp_arr(1:k)
    end subroutine remove_duplicates

end submodule euler_prob_0032_m

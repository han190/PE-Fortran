submodule(euler_interface_m) euler_prob_0032_m
    implicit none

contains

    module character(len=20) function euler0032()
        write (euler0032, "(i20)") ans()
    end function euler0032

    integer(int64) function ans()
        integer(int64) :: temp, i, j, k, arr_of_prods(9)
        integer(int64), allocatable :: arr_of_nonrepeats(:)

        k = 1
        do i = 1, 9
            do j = 1234, 9876
                if (digs_of_int(i*j) > 4) cycle
                temp = i*10**8 + j*10**4 + i*j

                if (is_pandigital(temp, digs_of_int(temp))) then
                    arr_of_prods(k) = i*j
                    k = k + 1
                end if
            end do
        end do

        do i = 12, 98
            do j = 123, 987
                if (digs_of_int(i*j) > 4) cycle
                temp = i*10**7 + j*10**4 + i*j
                if (is_pandigital(temp, digs_of_int(temp))) then
                    arr_of_prods(k) = i*j
                    k = k + 1
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

        tmp_arr = 0
        k = 1
        tmp_arr(1) = i_arr(1)

        do i = 2, size(i_arr)
            if (any(tmp_arr == i_arr(i))) cycle
            k = k + 1
            tmp_arr(k) = i_arr(i)
        end do

        allocate (o_arr(k))
        o_arr(:) = tmp_arr(1:k)
    end subroutine remove_duplicates

end submodule euler_prob_0032_m

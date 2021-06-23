submodule(euler_interface_m) euler_prob_0052_m
    implicit none

contains

    module character(len=20) function euler0052()
        write (euler0052, "(i20)") ans()
    end function euler0052

    integer function ans()
        integer :: i, j
        logical, dimension(0:9) :: arr, arr_tmp

        i = 1
        outer: do
            call digs_used_in_num(i, arr)

            inner: do j = 2, 6
                call digs_used_in_num(i*j, arr_tmp)

                if (all(arr .eqv. arr_tmp)) then
                    if (j == 6) exit outer
                else
                    exit inner
                end if
            end do inner

            i = i + 1
        end do outer

        ans = i
    end function ans

    subroutine digs_used_in_num(n, arr)
        integer, intent(in) :: n
        logical, intent(out) :: arr(0:9)
        integer, allocatable :: int_arr(:)
        integer :: i

        call int_2_arr(n, int_arr)
        arr = .false.

        do i = 1, digs_of_int(n)
            arr(int_arr(i)) = .true.
        end do
    end subroutine digs_used_in_num

end submodule euler_prob_0052_m

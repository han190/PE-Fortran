submodule(euler_interface_m) euler_prob_0038_m
    implicit none

contains

    module character(len=20) function euler0038()
        write (euler0038, "(i20)") ans()
    end function euler0038

    integer(int64) function ans()
        integer(int64), allocatable :: common_arr(:)
        integer(int64) :: i, j, tmp

        outer: do i = 2_int64, 10000_int64
            j = 1_int64; tmp = 0_int64

            inner: do
                if (tmp /= 0) then
                    if ( digs_of_int(tmp) > 9_int64 ) then
                        cycle outer
                    end if
                end if

                if ( is_pandigital(tmp) ) then
                   call append(common_arr, tmp)
                end if

                tmp = i * j + tmp * 10_int64**digs_of_int( i * j )
                j = j + 1_int64
            end do inner
        end do outer

        ans = maxval(common_arr, dim = 1)
    end function ans
end submodule euler_prob_0038_m

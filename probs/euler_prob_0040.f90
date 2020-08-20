submodule(euler_interface_m) euler_prob_0040_m
    implicit none

contains

    module character(len=20) function euler0040()
        write (euler0040, "(i20)") ans()
    end function euler0040

    integer function ans()
        integer, allocatable :: arr(:)
        integer :: dig_arr(6), d(6), i, s

        dig_arr = [0, 9, 189, 2889, 38889, 488889]
        d = [(1, i=1, 6)]

        do i = 2, 6
            associate ( &
                x => floor(real((10**i - dig_arr(i))/i), sp) &
                )
                d(i) = x + 10**(i - 1) - 1
            end associate

            call int_2_arr(d(i), arr)
            s = mod(10**i - dig_arr(i), i)
            d(i) = arr(s)
            deallocate (arr)
        end do

        ans = product(d)
    end function ans

end submodule euler_prob_0040_m

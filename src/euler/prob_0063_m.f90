submodule(euler_interface_m) euler_prob_0063_m

contains

    module character(len=20) function euler0063()
        write (euler0063, "(i20)") answer()
    end function euler0063

    function answer() result(ret)
        use euler_multiprecision_m
        implicit none

        integer :: ret, power, digs
        type(multiprecision_int_t) :: idx

        ret = 0
        idx = 1
        outer: do while (idx <= 9)
            power = 1
            inner: do
                digs = digits_of(idx**power)
                if (digs < power) then
                    exit inner
                else if (digs == power) then
                    ret = ret + 1
                end if
                power = power + 1
            end do inner
            idx = idx + 1
        end do outer
    end function answer

end submodule euler_prob_0063_m
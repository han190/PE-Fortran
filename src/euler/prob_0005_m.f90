submodule(euler_interface_m) euler_prob_0005_m
    implicit none

contains

    module character(len=20) function euler0005()
        write (euler0005, "(i20)") answer(20_int64)
    end function euler0005

    pure function answer(n) result(ret)
        integer(int64), intent(in) :: n
        integer(int64) :: i, ret

        ret = 1_int64
        do i = 1_int64, n
            ret = lcm(ret, i)
        end do
    end function answer

end submodule euler_prob_0005_m

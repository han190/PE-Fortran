submodule(euler_interface_m) euler_prob_0005_m
    implicit none

contains

    module character(len=20) function euler0005()
        write (euler0005, "(i20)") ans(20_int64)
    end function euler0005

    integer(int64) function ans(n)
        integer(int64), intent(in) :: n
        integer(int64) :: i, tmp

        tmp = 1_int64
        do i = 1_int64, n
            tmp = lcm(tmp, i)
        end do
        ans = tmp
    end function ans

end submodule euler_prob_0005_m

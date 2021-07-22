submodule(euler_interface_m) euler_prob_0028_m
    implicit none

contains

    module character(len=20) function euler0028()
        write (euler0028, "(i20)") ans()
    end function euler0028

    integer function ans()
        integer :: i, k, ur, bl, br, ul, tmp

        tmp = 1
        do i = 2, 501
            k = 2*i - 1
            ur = k**2
            bl = ur - 2*k + 2
            br = bl - (k - 1)
            ul = bl + (k - 1)
            tmp = tmp + ur + bl + br + ul
        end do
        ans = tmp
    end function ans

end submodule euler_prob_0028_m

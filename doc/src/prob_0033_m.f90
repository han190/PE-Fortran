submodule(euler_interface_m) euler_prob_0033_m
    implicit none

contains

    module character(len=20) function euler0033()
        write (euler0033, "(i20)") ans()
    end function euler0033

    integer function ans()
        integer :: c, d, n, dp, np

        c = 1; d = 1; n = 1
        np = 1; dp = 1

        do c = 1, 9
            do d = 1, c - 1
                do n = 1, d - 1
                    if ((n*10 + c)*d == (c*10 + d)*n) then
                        np = np*n
                        dp = dp*d
                    end if
                end do
            end do
        end do
        ans = dp/gcd(np, dp)
    end function ans

end submodule euler_prob_0033_m

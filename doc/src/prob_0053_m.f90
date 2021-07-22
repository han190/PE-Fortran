submodule(euler_interface_m) euler_prob_0053_m
    implicit none

contains

    module character(len=20) function euler0053()
        write (euler0053, "(i20)") ans(100)
    end function euler0053

    integer function ans(upper_bound)
        integer, intent(in) :: upper_bound
        integer :: count, n, r
        integer :: c(0:upper_bound, 0:upper_bound)
        integer, parameter :: b = 1000000

        count = 0
        do n = 1, upper_bound
            c(n, 0) = 1
            c(n, n) = 1

            do r = 1, n - 1
                c(n, r) = c(n - 1, r - 1) + c(n - 1, r)
                if (c(n, r) >= b) then
                    count = count + 1
                    c(n, r) = b
                end if
            end do

        end do

        ans = count
    end function ans
end submodule euler_prob_0053_m

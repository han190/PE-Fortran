submodule(euler_interface_m) euler_prob_0014_m
    implicit none

contains

    module character(len=20) function euler0014()
        write (euler0014, "(i20)") ans()
    end function euler0014

    integer(int64) function ans()
        integer(int64) :: k, i, tmp

        tmp = 0_int64; k = 0_int64
        do i = 500000_int64, 1000000_int64
            if (collatz(i) > tmp) then
                tmp = collatz(i)
                k = i
            end if
        end do
        ans = k
    end function ans

    integer(int64) function collatz(n)
        integer(int64), intent(in) :: n
        integer(int64) :: t, m

        t = 0_int64; m = n
        loop_1: do
            if (m == 1) exit loop_1
            if (mod(m, 2_int64) == 0_int64) then
                m = m/2_int64
            else
                m = 3_int64*m + 1_int64
            end if
            t = t + 1_int64
        end do loop_1
        collatz = t
    end function collatz

end submodule euler_prob_0014_m

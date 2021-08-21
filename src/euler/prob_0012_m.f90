submodule(euler_interface_m) euler_prob_0012_m
    implicit none

contains

    module character(len=20) function euler0012()
        write (euler0012, "(i20)") ans(500)
    end function euler0012

    function ans(n) result(ret)
        integer, intent(in) :: n
        integer :: ret, i

        i = 0
        ret = 0
        do
            i = i + 1
            ret = ret + i
            if (number_of_divisors(ret) > n) exit
        end do
    end function ans

    function number_of_divisors(val) result(ret)
        integer, intent(in) :: val
        integer :: ret, i, x

        ret = 0
        x = int(sqrt(real(val)))
        do concurrent (i = 1:x - 1)
            if (mod(val, i) == 0) ret = ret + 2
        end do
        if (mod(val, x) == 0) ret = ret + 1
    end function number_of_divisors

end submodule euler_prob_0012_m

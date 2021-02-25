submodule(euler_interface_m) euler_prob_0001_m
    implicit none

contains

    module character(len=20) function euler0001()
        write (euler0001, "(i20)") ans(3, 5, 1000)
    end function euler0001

    function ans(i, j, n) result(ret)
        integer, intent(in) :: i, j, n
        integer :: ret

        ret = sum_divisibly_by(i, n - 1) &
            + sum_divisibly_by(j, n - 1) &
            - sum_divisibly_by(i*j, n - 1)
    end function ans

    function sum_divisibly_by(i, j) result(ret)
        integer, intent(in) :: i, j
        integer :: ret

        ret = i*(j/i*(j/i + 1))/2
    end function sum_divisibly_by

end submodule euler_prob_0001_m

submodule(euler_interface_m) euler_prob_0002_m
    implicit none

contains

    module character(len=20) function euler0002()
        write (euler0002, "(i20)") answer(4000000)
    end function euler0002

    pure function answer(n) result(ret)
        integer, intent(in) :: n
        integer :: ret, i

        i = 0; ret = 0
        do while (fibonacci(i) <= n)
            ret = ret + fibonacci(i)
            i = i + 3
        end do
    end function answer

end submodule euler_prob_0002_m

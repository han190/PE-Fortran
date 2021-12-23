submodule(interface_m) euler_problem_0025_m

contains

    module character(len=20) function euler0025()
        write (euler0025, "(i20)") answer()
    end function euler0025

    elemental integer(i32) function answer()
        use big_integer_m
        implicit none

        type(big_integer) :: fibo_seq(2)
        integer(i32) :: i

        answer = 2
        fibo_seq(1) = 1; fibo_seq(2) = 1
        outer: do
            inner: do i = 1, 2
                fibo_seq(i) = fibo_seq(1) + fibo_seq(2)
                answer = answer + 1
                if (size(fibo_seq(i)%arr) >= 1000) exit outer
            end do inner
        end do outer
    end function answer

end submodule euler_problem_0025_m

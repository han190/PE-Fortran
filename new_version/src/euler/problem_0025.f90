submodule(interface_m) euler_problem_0025_m

contains

    module character(len=20) function euler0025()
        write (euler0025, "(i20)") answer()
    end function euler0025

    pure integer(i32) function answer()
        use multiprecision_m
        implicit none

        type(multiprecision_t) :: fibonacci_seq(2)
        integer(i32) :: i

        answer = 2
        fibonacci_seq(1) = 1; fibonacci_seq(2) = 1
        outer: do
            inner: do i = 1, 2
                fibonacci_seq(i) = fibonacci_seq(1) + fibonacci_seq(2)
                answer = answer + 1
                if (number_of_digits(fibonacci_seq(i)) >= 1000) exit outer
            end do inner
        end do outer
    end function answer

end submodule euler_problem_0025_m

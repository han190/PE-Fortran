submodule(interface_m) euler_problem_0020_m
    implicit none

contains

    module character(len=20) function euler0020()
        write (euler0020, "(i20)") answer()
    end function euler0020

    pure integer(i32) function answer()
        use multiprecision_m, only: multiprecision_t, to_long
        implicit none

        type(multiprecision_t) :: n
        integer(i32) :: i

        n = 1
        do i = 1, 100
            n = n*i
        end do
        answer = sum(n%arr)
    end function answer

end submodule euler_problem_0020_m

submodule(interface_m) euler_problem_0005_m
    implicit none

contains

    module pure character(len=20) function euler0005()
        write (euler0005, "(i20)") answer()
    end function euler0005

    pure integer(i64) function answer()
        integer(i64), parameter :: n = 20
        integer(i64) :: i

        answer = 1
        do i = 1, n
            answer = lcm(answer, i)
        end do
    end function answer

end submodule euler_problem_0005_m

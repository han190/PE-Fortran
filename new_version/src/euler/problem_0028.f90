submodule(interface_m) euler_problem_0028_m
    implicit none

contains

    module character(len=20) function euler0028()
        write (euler0028, "(i20)") answer()
    end function euler0028

    pure integer(i32) function answer()
        integer(i32) :: i, j

        answer = sum([([(2*(1 - i)*j, j=0, 3)] + (2*i - 1)**2, i=2, 501)]) + 1
    end function answer

end submodule euler_problem_0028_m

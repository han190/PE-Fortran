submodule(interface_m) euler_problem_0053_m
    implicit none

contains

    module character(len=20) function euler0053()
        write (euler0053, "(i20)") answer()
    end function euler0053

    elemental integer(i32) function answer()
        integer(i32), parameter :: upper = 100
        integer(i32), parameter :: b = 1000000
        integer(i32) :: n, r, c(0:upper, 0:upper)

        answer = 0
        do n = 1, upper
            c(n, 0) = 1
            c(n, n) = 1
            do r = 1, n - 1
                c(n, r) = c(n - 1, r - 1) + c(n - 1, r)
                if (c(n, r) >= b) then
                    answer = answer + 1
                    c(n, r) = b
                end if
            end do
        end do
    end function answer

end submodule euler_problem_0053_m

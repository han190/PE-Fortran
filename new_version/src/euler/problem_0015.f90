submodule(interface_m) euler_problem_0015_m
    implicit none

contains

    module character(len=20) function euler0015()
        write (euler0015, "(i20)") answer()
    end function euler0015

    elemental integer(i64) function answer()
        integer(i32) :: i

        associate (n => 20_i32)
            answer = 1
            do i = 1, n
                answer = answer*(n + i)/i
            end do
        end associate
    end function answer

end submodule euler_problem_0015_m

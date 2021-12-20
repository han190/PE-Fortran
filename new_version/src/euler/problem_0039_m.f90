submodule(interface_m) euler_problem_0039_m
    implicit none

contains

    module character(len=20) function euler0039()
        write (euler0039, "(i20)") answer()
    end function euler0039

    pure integer(i32) function answer()
        integer(i32), parameter :: n = 1000
        integer(i32) :: i

        answer = maxloc([(right_triangle(i), i=1, n)], dim=1)
    end function answer

    pure integer(i32) function right_triangle(p)
        integer(i32), intent(in) :: p
        integer(i32) :: i, j, k

        right_triangle = 0
        do i = 1, p/2 + 1
            do j = 1, p - i - 1
                k = p - i - j
                if (i**2 + j**2 == k**2) right_triangle = right_triangle + 1
            end do
        end do
    end function right_triangle

end submodule euler_problem_0039_m

submodule(interface_m) euler_problem_0009_m
    implicit none

contains

    module character(len=20) function euler0009()
        write (euler0009, "(i20)") answer()
    end function euler0009

    pure integer(i32) function answer()
        integer(i32), parameter :: n = 1000
        integer(i32) :: i, j

        answer = 0
        do i = 1, n
            do j = i + 1, n
                associate (k => n - i - j)
                    if (.not. j < k) cycle
                    if (i**2 + j**2 == k**2) then
                        answer = i*j*k
                        exit
                    end if
                end associate
            end do
        end do
    end function answer

end submodule euler_problem_0009_m

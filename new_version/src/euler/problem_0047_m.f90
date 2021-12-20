submodule(interface_m) euler_problem_0047_m
    implicit none

contains

    module character(len=20) function euler0047()
        write (euler0047, "(i20)") answer()
    end function euler0047

    pure integer(i32) function answer()
        integer(i32), parameter :: n = 1000000 - 1
        integer(i32) :: i, goal(4)
        integer(i32), allocatable :: n_factor(:)

        allocate (n_factor(n + 1)); n_factor = 0
        do i = 2, n
            if (n_factor(i) == 0) n_factor(2*i:n:i) = n_factor(2*i:n:i) + 1
        end do

        goal = 4
        do answer = 2, n
            if (all(n_factor(answer:answer + 3) == goal)) exit
        end do
    end function answer

end submodule euler_problem_0047_m

submodule(interface_m) euler_problem_0007_m
    implicit none

contains

    module character(len=20) function euler0007()
        write (euler0007, "(i20)") answer()
    end function euler0007

    elemental integer(i32) function answer()
        use prime_m, only: is_prime
        implicit none

        integer(i32), parameter :: n = 10000
        integer(i32) :: i

        i = 0
        answer = 0
        do while (i <= n)
            answer = answer + 1
            if (mod(answer, 2) == 2 .and. answer /= 2) then
                cycle
            else if (is_prime(answer)) then
                i = i + 1
            end if
        end do
    end function answer

end submodule euler_problem_0007_m

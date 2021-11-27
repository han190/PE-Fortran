submodule(interface_m) euler_problem_0021_m
    implicit none

contains

    module pure character(len=20) function euler0021()
        write (euler0021, "(i20)") answer()
    end function euler0021

    pure integer(i32) function answer()
        integer(i32), parameter :: n = 10000
        integer(i32) :: i

        answer = 0
        do i = 1, n
            if (spd(spd(i)) == i .and. spd(i) /= i) answer = answer + i
        end do
    end function answer

    !> Sum of proper divisors.
    pure integer(i32) function spd(n)
        integer(i32), intent(in) :: n
        integer(i32) :: i

        spd = 1
        do i = 2, int(sqrt(real(n, sp)))
            if (mod(n, i) == 0) spd = spd + i + n/i
        end do
    end function spd

end submodule euler_problem_0021_m

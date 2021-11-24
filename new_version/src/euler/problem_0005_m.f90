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

    pure integer(i64) function lcm(n1, n2)
        integer(i64), intent(in) :: n1, n2

        lcm = n1*n2/gcd(n1, n2)
    end function lcm

    pure recursive function gcd(n1, n2) result(ret)
        integer(i64), intent(in) :: n1, n2
        integer(i64) :: ret

        select case (n2)
        case (0)
            ret = n1
        case default
            ret = gcd(n2, mod(n1, n2))
        end select
    end function gcd

end submodule euler_problem_0005_m

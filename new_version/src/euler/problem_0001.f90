submodule(interface_m) euler_problem_0001_m
    implicit none

contains

    module pure character(len=20) function euler0001()
        write (euler0001, "(i20)") answer()
    end function euler0001

    pure integer(i32) function answer()
        integer(i32), parameter :: i = 3, j = 5, n = 1000

        answer = sdb(i, n - 1) + sdb(j, n - 1) - sdb(i*j, n - 1)
    end function answer

    !> Sum divisibly by.
    pure function sdb(i, j) result(ret)
        integer(i32), intent(in) :: i, j
        integer(i32) :: ret

        ret = i*(j/i*(j/i + 1))/2
    end function sdb

end submodule euler_problem_0001_m

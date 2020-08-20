submodule(euler_interface_m) euler_prob_0001_m
    implicit none

contains

    module character(len=20) function euler0001()
        write (euler0001, "(i20)") ans(3, 5, 1000)
    end function euler0001

    integer function ans(i, j, tmp)
        integer, intent(in) :: i, j, tmp

        ans = sdb(i, tmp - 1) + sdb(j, tmp - 1) - sdb(i*j, tmp - 1)
    end function ans

    integer function sdb(i, j) ! sum divisibly by
        integer, intent(in) :: i, j

        sdb = i*(j/i*(j/i + 1))/2
    end function sdb

end submodule euler_prob_0001_m

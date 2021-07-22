submodule(euler_interface_m) euler_prob_0045_m
    implicit none

contains

    module character(len=20) function euler0045()
        write (euler0045, "(i20)") ans()
    end function euler0045

    integer(int64) function ans()
        integer(int64) :: x, y

        call hex_n_pen(3, x, y)
        ans = y*(2_int64*y - 1_int64)
    end function ans

    recursive subroutine hex_n_pen(i, x, y)
        integer, intent(in) :: i
        integer(int64), intent(out) :: x, y
        integer(int64) :: a, b

        if (i == 1) then
            x = 1_int64
            y = 1_int64
        else
            call hex_n_pen(i - 1, a, b)
            x = 97_int64*a + 112_int64*b - 44_int64
            y = 84_int64*a + 97_int64*b - 38_int64
        end if
    end subroutine hex_n_pen

end submodule euler_prob_0045_m

submodule(euler_interface_m) euler_prob_0004_m
    implicit none

contains

    module character(len=20) function euler0004()
        write (euler0004, "(i20)") answer(999)
    end function euler0004

    pure function answer(n) result(ret)
        integer, intent(in) :: n
        integer :: i, j, ret

        ret = 0
        do concurrent (i = 100:n, j = 100:n)
            if (is_palindromic(i*j) .and. i*j > ret) ret = i*j
        end do
    end function answer
    
end submodule euler_prob_0004_m

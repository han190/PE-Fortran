submodule(euler_interface_m) euler_prob_0009_m
    implicit none

contains

    module character(len=20) function euler0009()
        write (euler0009, "(i20)") answer(1000)
    end function euler0009

    integer function answer(n)
        integer, intent(in) :: n
        integer :: i, j, k

        answer = 0
        outer: do i = 1, n
            inner: do j = i + 1, n
                k = n - i - j
                if (i**2 + j**2 == k**2) then
                    answer = i*j*k
                    return
                end if
            end do inner
        end do outer
    end function answer

end submodule euler_prob_0009_m

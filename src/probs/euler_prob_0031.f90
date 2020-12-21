submodule(euler_interface_m) euler_prob_0031_m
    implicit none

contains

    module character(len=20) function euler0031()
        write (euler0031, "(i20)") ans(200)
    end function euler0031

    integer function ans(n)
        integer, intent(in) :: n
        integer :: coins(0:7), i, j
        integer :: ans_arr(0:7, 0:n)

        coins = [1, 2, 5, 10, 20, 50, 100, 200]
        ans_arr(1:7, :) = 0
        ans_arr(0, :) = 1

        outer: do j = 0, n
            inner: do i = 1, 7
                associate ( &
                    next => ans_arr(i, j), &
                    prev => ans_arr(i - 1, j) &
                    )
                    if (j < coins(i)) then
                        next = prev
                    else
                        next = prev + ans_arr(i, j - coins(i))
                    end if
                end associate
            end do inner
        end do outer
        ans = ans_arr(7, n)
    end function ans

end submodule euler_prob_0031_m

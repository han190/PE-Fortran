submodule(euler_interface_m) euler_prob_0047_m
    implicit none

contains

    module character(len=20) function euler0047()
        write (euler0047, "(i20)") ans(1000000)
    end function euler0047

    integer function ans(n)
        integer, intent(in) :: n
        integer :: i, j, n_factor(n), goal(4)

        n_factor = 0

        outer: do i = 2, n - 1

            if (n_factor(i) == 0) then
                inner: do j = 2*i, n - 1, i
                    n_factor(j) = n_factor(j) + 1
                end do inner
            end if

        end do outer

        goal = [4, 4, 4, 4]

        do i = 2, n - 1
            if (all(n_factor(i:i + 3) == goal(:))) exit
        end do

        ans = i
    end function ans
end submodule euler_prob_0047_m

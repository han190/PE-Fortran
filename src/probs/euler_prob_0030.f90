submodule(euler_interface_m) euler_prob_0030_m
    implicit none

contains

    character(len=20) function euler0030()
        write (euler0030, "(i20)") ans(999999)
    end function euler0030

    integer function ans(n)
        integer, intent(in) :: n
        integer, allocatable :: arr(:)
        integer :: tmp, i

        tmp = 0
        do i = 2, n
            call int_2_arr(i, arr)

            if (sum(arr**5) == i) then
                tmp = tmp + i
            end if
        end do
        ans = tmp
    end function ans

end submodule euler_prob_0030_m

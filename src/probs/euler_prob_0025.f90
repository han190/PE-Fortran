submodule(euler_interface_m) euler_prob_0025_m
    implicit none 

contains 

    module character(len=20) function euler0025()
        write (euler0025, "(i20)") ans(1000)
    end function euler0025

    integer function ans(n)
        integer, intent(in) :: n 
        integer :: int_arr1(n), int_arr2(n)
        integer :: tmp(n), j, k

        int_arr1 = 0; int_arr2 = 0
        int_arr1(n) = 1; int_arr2(n) = 1
        k = 3

        loop_1: do

            tmp = int_arr1 + int_arr2
            loop_3: do j = n, 2, -1
                call carry(tmp(j - 1:j))
            end do loop_3
            int_arr1 = tmp 

            if (int_arr1(1) /= 0) exit loop_1
            k = k + 1

            tmp = int_arr1 + int_arr2
            loop_6: do j = n, 2, -1
                call carry(tmp(j - 1:j))
            end do loop_6
            int_arr2 = tmp 

            if (int_arr2(1) /= 0) exit loop_1
            k = k + 1

        end do loop_1

        ans = k
    end function ans 

    subroutine carry(a)
        integer, intent(inout) :: a(2)
        integer :: tmp 

        tmp = a(2) 
        a(2) = a(2) - a(2) / 10 * 10 
        a(1) = a(1) + ( tmp - a(2) ) / 10 
    end subroutine carry
end submodule euler_prob_0025_m
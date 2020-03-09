submodule(euler_interface_m) euler_prob_0048_m
    implicit none 

contains 

    module character(len=20) function euler0048()
        write (euler0048, "(a20)") trim(ans())
    end function euler0048

    character(len=20) function ans()
        integer :: isum(10), i, iarr(10)
        
        isum = 0 

        do i = 1, 1000
            call self_pow10(i, iarr)
            call add10(isum, iarr)
        end do 

        write (ans, "(20(i1))") isum
    end function ans

    subroutine add10(a, b)
        integer, intent(inout) :: a(:)
        integer, intent(in) :: b(:)
        integer :: j

        a(:) = a(:) + b(:)

        do j = 10, 2, -1
            call carry_add( a(j - 1:j) )
        end do 

        a(1) = unit_digit( a(1) )
    end subroutine add10

    subroutine self_pow10(n, iarr)
        integer, intent(in) :: n
        integer, intent(out) :: iarr(:)
        integer :: i, j

        iarr(:) = 0
        iarr(10) = n

        do i = 1, n - 1
            iarr(10) = iarr(10) *n

            do j = 10, 2, -1
                call carry_mul( iarr(j - 1:j), n )
            end do 

            iarr(1) = unit_digit( iarr(1) )
        end do 
    end subroutine self_pow10

    subroutine carry_mul(a, m)
        integer, intent(inout) :: a(:) !a(2)
        integer, intent(in) :: m 
        integer :: tmp 

        tmp = a(2)
        a(2) = unit_digit( a(2) )
        a(1) = a(1) * m + ( tmp - a(2) ) / 10
    end subroutine carry_mul

    subroutine carry_add(a)
        integer, intent(inout) :: a(:) !a(2)
        integer :: tmp 

        tmp = a(2)
        a(2) = unit_digit( a(2) )
        a(1) = a(1) + ( tmp - a(2) ) / 10
    end subroutine carry_add

end submodule euler_prob_0048_m

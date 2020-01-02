submodule(euler_interface_m) euler_prob_0017_m 
    implicit none 

contains 

    module character(len=20) function euler0017()
        write (euler0017, "(i20)") ans()
    end function euler0017 

    integer function ans()
        integer :: tot_c, i  

        tot_c = 0
        do i = 1, 9
            tot_c = tot_c + count_letters(i)
        end do

        do i = 10, 19
            tot_c = tot_c + count_letters10(i)
        end do

        do i = 20, 99
            tot_c = tot_c + count_letters10(i)
        end do

        do i = 100, 999
            tot_c = tot_c + count_letters100(i)
        end do
        ans = tot_c + 11
    end function ans 

    integer function count_letters(n)
        integer, intent(in) :: n
        integer :: temp
    
        select case (n)
        case (0)
            temp = 0
        case (1, 2, 6, 10)
            temp = 3
        case (4, 5, 9)
            temp = 4
        case (3, 7, 8, 40, 50, 60)
            temp = 5
        case (11, 12, 20, 30, 80, 90)
            temp = 6
        case (15, 16, 70)
            temp = 7
        case (13, 14, 18, 19)
            temp = 8
        case (17)
            temp = 9
        case (1000)
            temp = 8
        case default
            temp = 1000
        end select
        count_letters = temp
    end function count_letters

    integer function count_letters10(n)
        integer, intent(in) :: n
        integer :: d1, d2

        d1 = 0; d2 = 0
        if ( count_letters(n) /= 1000 ) then
            count_letters10 = count_letters(n)
        else
            d1 = ( n / 10 ) * 10
            d2 = n - d1
            count_letters10 =                                                  &
                count_letters(d1) + count_letters(d2)
        end if

        if ( n == 0 ) then
            count_letters10 = 0
        end if
    end function count_letters10

    integer function count_letters100(n)
        integer, intent(in) :: n
        integer :: d1, d2, hun, and

        hun = 7; and = 3
        if ( mod(n, 100) == 0 ) then
            d1 = n / 100
            count_letters100 = count_letters(d1) + hun
        else
            d1 = n / 100
            d2 = n - ( n / 100 ) * 100
            count_letters100 = count_letters(d1) +                             &
                hun + and + count_letters10(d2)
        end if
    end function count_letters100

end submodule euler_prob_0017_m
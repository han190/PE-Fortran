submodule(euler_interface_m) euler_prob_0034_m
    implicit none

contains

    module character(len=20) function euler0034()
        write (euler0034, "(i20)") ans(40585)
    end function euler0034

    integer function ans(n)
        integer, intent(in) :: n
        integer :: i, isum

        isum = 0
        do i = 1, n
            if (is_curious(i)) then
                isum = isum + i
            end if
        end do

        ans = isum
    end function ans

    logical function is_curious(n)
        integer, intent(in) :: n
        integer, allocatable :: arr(:)
        integer :: i, tmp

        if (n == 1 .or. n == 2) then
            is_curious = .false.
            return
        end if

        tmp = 0
        is_curious = .false.
        call int_2_arr(n, arr)

        do i = 1, size(arr)
            tmp = tmp + factorial(arr(i))
        end do

        if (tmp == n) is_curious = .true.
    end function is_curious

end submodule euler_prob_0034_m

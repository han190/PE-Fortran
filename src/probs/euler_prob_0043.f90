submodule(euler_interface_m) euler_prob_0043_m
    implicit none
    integer(int64) :: answer

contains

    module character(len=20) function euler0043()
        write (euler0043, "(i20)") ans()
    end function euler0043

    integer(int64) function ans()
        integer(int64) :: test_arr(10)

        test_arr = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        call permute(10_int64, test_arr, 1_int64, 10_int64)
        ans = answer
    end function ans

    recursive subroutine permute(n, a, l, r)
        integer(int64), intent(in) :: n, l, r
        integer(int64), intent(inout) :: a(n)
        integer(int64) :: i, tmp

        if (l == r) then
            call arr_2_int(a, tmp)

            if (is_subinteger_divisible(tmp)) then
                answer = answer + tmp
            end if
            return
        else
            do i = l, r
                call swap(a(i), a(l))
                call permute(10_int64, a, l + 1_int64, r)
                call swap(a(i), a(l))
            end do
        end if
    end subroutine permute

    logical function is_subinteger_divisible(n)
        integer(int64), intent(in) :: n
        integer(int64) :: i, tmp, three_int, prime_arr(7)

        prime_arr = [17, 13, 11, 7, 5, 3, 2]
        tmp = n

        is_subinteger_divisible = .true.
        do i = 1, 7
            three_int = tmp - tmp/1000*1000
            if (mod(three_int, prime_arr(i)) /= 0) then
                is_subinteger_divisible = .false.
                return
            end if

            tmp = tmp/10
        end do
    end function is_subinteger_divisible

end submodule euler_prob_0043_m

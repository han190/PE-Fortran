submodule(euler_interface_m) euler_prob_0036_m
    implicit none

contains

    module character(len=20) function euler0036()
        write (euler0036, "(i20)") ans(1000000_int64)
    end function euler0036

    integer(int64) function ans(n)
        integer(int64), intent(in) :: n
        integer(int64) :: isum, i, next_ten_pow, p

        isum = 0_int64; i = 1_int64
        next_ten_pow = 10_int64
        p = gen_single_palindormic_10(i)

        do while (p <= n)
            if (is_palindromic_2(p)) then
                isum = isum + p
            end if

            p = p + (i - i/10_int64)*next_ten_pow
            if (p <= n .and. is_palindromic_2(p)) isum = isum + p

            i = i + 1_int64
            if (i == next_ten_pow) then
                next_ten_pow = next_ten_pow*10_int64
            end if
            p = gen_single_palindormic_10(i)
        end do
        ans = isum
    end function ans

    integer(int64) function gen_single_palindormic_10(n)
        integer(int64), intent(in) :: n
        integer(int64) :: tmp, i

        tmp = n
        i = n/10_int64

        do while (i > 0_int64)
            tmp = tmp*10_int64 + mod(i, 10_int64)
            i = i/10_int64
        end do

        gen_single_palindormic_10 = tmp
    end function gen_single_palindormic_10

    logical function is_palindromic_2(n)
        integer(int64), intent(in) :: n
        integer(int64) :: a, b, digit

        a = n
        b = 0_int64
        do while (a > 0_int64)
            digit = mod(a, 2_int64)
            b = b*2_int64 + digit
            a = a/2_int64
        end do

        if (n == b) then
            is_palindromic_2 = .true.
        else
            is_palindromic_2 = .false.
        end if
    end function is_palindromic_2
end submodule euler_prob_0036_m

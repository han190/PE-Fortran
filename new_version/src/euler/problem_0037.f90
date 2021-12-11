submodule(interface_m) euler_problem_0037_m
    implicit none

contains

    module character(len=20) function euler0037()
        write (euler0037, "(i20)") answer()
    end function euler0037

    pure integer(i64) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i64), parameter :: n = 1000000_i64
        integer(i64) :: i, knt
        logical, allocatable :: is_prime(:)

        call Sieve_of_Eratosthenes(n, is_prime)
        i = 10; knt = 0; answer = 0

        do
            if (knt == 11 .or. i == n) exit
            if (is_prime(i) .and. is_truncatable(i, is_prime)) then
                answer = answer + i
                knt = knt + 1
            end if
            i = i + 1
        end do
    end function answer

    pure logical function is_truncatable(n, is_prime)
        integer(i64), intent(in) :: n
        logical, intent(in) :: is_prime(:)

        if (is_left_truncatable(n, is_prime) .and. &
            is_right_truncatable(n, is_prime)) then
            is_truncatable = .true.
        else
            is_truncatable = .false.
        end if
    end function is_truncatable

    pure logical function is_left_truncatable(n, is_prime)
        integer(i64), intent(in) :: n
        logical, intent(in) :: is_prime(:)
        integer(i64) :: temp

        temp = 10_i64
        do while (temp < n .and. mod(n, temp) >= 1)
            if (.not. is_prime(mod(n, temp))) then
                is_left_truncatable = .false.
                return
            end if
            temp = temp*10_i64
        end do
        is_left_truncatable = .true.
    end function is_left_truncatable

    pure logical function is_right_truncatable(n, is_prime)
        integer(i64), intent(in) :: n
        logical, intent(in) :: is_prime(:)
        integer(i64) :: temp

        temp = n
        do while (temp > 0_i64)
            if (is_prime(temp)) then
                temp = temp/10_i64
            else
                is_right_truncatable = .false.
                return
            end if
        end do
        is_right_truncatable = .true.
    end function is_right_truncatable

end submodule euler_problem_0037_m

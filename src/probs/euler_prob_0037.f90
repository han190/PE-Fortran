submodule(euler_interface_m) euler_prob_0037_m
    implicit none

contains

    module character(len=20) function euler0037()
        write (euler0037, "(i20)") ans(1000000_int64)
    end function euler0037

    integer(int64) function ans(n)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none
        integer(int64), intent(in) :: n
        integer(int64) :: i, isum, icount
        logical, allocatable :: is_prime(:)

        call sieve_of_Eratosthenes(n, is_prime)
        i = 10_int64
        isum = 0_int64
        icount = 0_int64

        do
            if (                                                               &
                icount == 11_int64 .or.                                        &
                i == n                                                 &
            ) exit

            if (                                                               &
                is_prime(i) .and.                                              &
                is_truncatable(i, is_prime)                                    &
            ) then
                isum = isum + i
                icount = icount + 1_int64
            end if
            i = i + 1_int64
        end do

        ans = isum
    end function ans

    logical function is_truncatable(n, is_prime)
        integer(int64), intent(in) :: n
        logical, allocatable, intent(in) :: is_prime(:)

        is_truncatable = .false.
        if (                                                                   &
            is_left_truncatable(n, is_prime) .and.                             &
            is_right_truncatable(n, is_prime)                                  &
        ) is_truncatable = .true.
    end function is_truncatable

    logical function is_left_truncatable(n, is_prime)
        integer(int64), intent(in) :: n
        logical, allocatable, intent(in) :: is_prime(:)
        integer(int64) :: tmp

        tmp = 10_int64
        do while ( tmp < n )
            if ( .not. is_prime( mod(n, tmp) ) ) then
                is_left_truncatable = .false.
                return
            end if
            tmp = tmp * 10_int64
        end do
        is_left_truncatable = .true.
    end function is_left_truncatable

    logical function is_right_truncatable(n, is_prime)
        integer(int64), intent(in) :: n
        logical, allocatable, intent(in) :: is_prime(:)
        integer(int64) :: tmp

        tmp = n
        do while ( tmp > 0_int64 )
            if ( is_prime(tmp) ) then
                tmp = tmp / 10_int64
            else
                is_right_truncatable = .false.
                return
            end if
        end do
        is_right_truncatable = .true.
    end function is_right_truncatable

end submodule euler_prob_0037_m

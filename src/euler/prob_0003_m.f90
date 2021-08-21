submodule(euler_interface_m) euler_prob_0003_m
    implicit none

contains

    module character(len=20) function euler0003()
        write (euler0003, "(i20)") answer(600851475143_int64)
    end function euler0003

    pure function answer(n) result(ret)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none

        integer(int64), intent(in) :: n
        logical, allocatable :: is_prime(:)
        integer(int64) :: max_, ret

        max_ = int(sqrt(real(n, sp)) + 1._sp, int64)
        call sieve_of_Eratosthenes(max_, is_prime)
        do ret = max_, 2_int64, -1_int64
            if (is_prime(ret) .and. mod(n, ret) == 0_int64) exit
        end do
    end function answer

end submodule euler_prob_0003_m

submodule(euler_interface_m) euler_prob_0003_m
    implicit none

contains

    module character(len=20) function euler0003()
        write (euler0003, "(i20)") ans(600851475143_int64)
    end function euler0003

    integer(int64) function ans(n)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none
        integer(int64), intent(in) :: n
        logical, allocatable :: is_prime(:)
        integer(int64) :: fmax, i

        fmax = int(sqrt(real(n, sp)) + 1.0_sp, int64)
        call sieve_of_Eratosthenes(fmax, is_prime)
        do i = fmax, 2_int64, -1_int64
            if (is_prime(i) .and. mod(n, i) == 0_int64) exit
        end do
        ans = i
    end function ans
end submodule euler_prob_0003_m

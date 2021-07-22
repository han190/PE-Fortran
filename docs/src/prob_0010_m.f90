submodule(euler_interface_m) euler_prob_0010_m
    implicit none

contains

    module character(len=20) function euler0010()
        write (euler0010, "(i20)") ans(2000000_int64)
    end function euler0010

    integer(int64) function ans(n)
        use euler_primes_m, only: sieve_of_eratosthenes
        implicit none
        integer(int64), intent(in) :: n
        integer(int64) :: i, tmp
        logical, allocatable :: is_prime(:)

        tmp = 0_int64
        call sieve_of_eratosthenes(n, is_prime)

        ! ans = sum( pack([ (i, i = 0_int64, n) ], is_prime) )
        do i = 0_int64, n
            if (is_prime(i)) then
                tmp = tmp + i
            end if
        end do
        ans = tmp
    end function ans
end submodule euler_prob_0010_m

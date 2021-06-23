submodule(euler_interface_m) euler_prob_0041_m
    implicit none

contains

    module character(len=20) function euler0041()
        write (euler0041, "(i20)") ans(7654321)
    end function euler0041

    integer function ans(n)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none
        integer, intent(in) :: n
        logical, allocatable :: is_prime(:)
        integer :: i

        call sieve_of_Eratosthenes(n, is_prime)
        do i = n, 1, -1
            if (is_prime(i) .and. is_pandigital(i, digs_of_int(i))) exit
        end do

        ans = i
    end function ans
end submodule euler_prob_0041_m

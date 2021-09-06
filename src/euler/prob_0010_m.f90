submodule(euler_interface_m) euler_prob_0010_m
    implicit none

contains

    module character(len=20) function euler0010()
        write (euler0010, "(i20)") answer(2000000_int64)
    end function euler0010

    function answer(n) result(ret)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none
        integer(int64), intent(in) :: n
        integer(int64) :: i, ret
        logical, allocatable :: is_prime(:)

        call sieve_of_Eratosthenes(n, is_prime)
        do i = 0_int64, n
            if (is_prime(i)) ret = ret + i
        end do
    end function answer
end submodule euler_prob_0010_m

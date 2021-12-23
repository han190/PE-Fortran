submodule(interface_m) euler_problem_0003_m
    implicit none

contains

    module character(len=20) function euler0003()
        write (euler0003, "(i20)") answer()
    end function euler0003

    pure integer(i64) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i64), parameter :: n = 600851475143_i64
        integer(i64), allocatable :: primes(:)
        integer(i64) :: i

        call Sieve_of_Eratosthenes(sqrt(n), primes)
        do i = size(primes), 1, -1
            if (mod(n, primes(i)) == 0) exit
        end do
        answer = primes(i)
    end function answer

end submodule euler_problem_0003_m

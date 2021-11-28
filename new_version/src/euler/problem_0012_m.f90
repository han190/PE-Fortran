submodule(interface_m) euler_problem_0012_m
    implicit none

contains

    module pure character(len=20) function euler0012()
        write (euler0012, "(i20)") answer()
    end function euler0012

    pure integer(i32) function answer()
        use prime_m, only: Sieve_of_Sundaram
        implicit none

        integer(i32), parameter :: n = 500
        integer(i32), parameter :: limit = 20
        integer(i32), allocatable :: primes(:)
        logical, allocatable :: is_prime(:)
        integer(i32) :: i

        call Sieve_of_Sundaram(limit, is_prime)
        primes = [2, pack([(i*2 + 1, i=1, size(is_prime))], is_prime)]

        i = 0
        answer = 0
        do
            i = i + 1
            answer = answer + i
            if (number_of_divisors(answer, primes) > n) exit
        end do
    end function answer

end submodule euler_problem_0012_m

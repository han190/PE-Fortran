submodule(interface_m) euler_problem_0012_m
    implicit none

contains

    module pure character(len=20) function euler0012()
        write (euler0012, "(i20)") answer()
    end function euler0012

    pure integer(i32) function answer()
        use prime_m, only: get_primes
        implicit none

        integer(i32), parameter :: n = 500
        integer(i32), parameter :: limit = 20
        integer(i32), allocatable :: primes(:)
        integer(i32) :: i

        primes = get_primes(limit, algorithm="Sieve of Sundaram")

        i = 0; answer = 0
        do
            i = i + 1
            answer = answer + i
            if (number_of_divisors(answer, primes) > n) exit
        end do
    end function answer

end submodule euler_problem_0012_m

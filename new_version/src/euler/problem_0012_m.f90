submodule(interface_m) euler_problem_0012_m
    implicit none

contains

    module character(len=20) function euler0012()
        write (euler0012, "(i20)") answer()
    end function euler0012

    pure integer(i32) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i32), parameter :: n = 500
        integer(i32), parameter :: upper_ = 20
        integer(i32), allocatable :: primes(:)
        integer(i32) :: i

        call Sieve_of_Eratosthenes(upper_, primes)
        i = 0; answer = 0
        do
            i = i + 1
            answer = answer + i
            if (number_of_divisors(answer, primes) > n) exit
        end do
    end function answer

end submodule euler_problem_0012_m

submodule(interface_m) euler_problem_0003_m
    implicit none

contains

    module pure character(len=20) function euler0003()
        write (euler0003, "(i20)") answer()
    end function euler0003

    pure integer(i64) function answer()
        use prime_m, only: sieve_of_Eratosthenes
        implicit none

        integer(i64), parameter :: n = 600851475143_i64
        logical, allocatable :: is_prime(:)

        associate (upper_bound => int(sqrt(real(n, sp)) + 1._sp, i64))
            call sieve_of_Eratosthenes(upper_bound, is_prime)
            do answer = upper_bound, 2, -1
                if (is_prime(answer) .and. mod(n, answer) == 0) exit
            end do
        end associate
    end function answer

end submodule euler_problem_0003_m

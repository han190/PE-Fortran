submodule(interface_m) euler_problem_0010_m
    implicit none

contains

    module character(len=20) function euler0010()
        write (euler0010, "(i20)") answer()
    end function euler0010

    pure integer(i64) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i32), parameter :: n = 2000000_i32
        integer(i32), allocatable :: primes(:)
        
        call Sieve_of_Eratosthenes(n, primes)
        answer = sum(primes)
    end function answer

end submodule euler_problem_0010_m

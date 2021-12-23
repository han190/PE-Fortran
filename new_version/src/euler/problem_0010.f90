submodule(interface_m) euler_problem_0010_m
    implicit none

contains

    module character(len=20) function euler0010()
        write (euler0010, "(i20)") answer()
    end function euler0010

    elemental integer(i64) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i64), parameter :: n = 2000000_i64
        logical, allocatable :: is_prime(:)
        integer(i64) :: i
        
        allocate(is_prime(n))
        call Sieve_of_Eratosthenes(n, is_prime)
        answer = 0
        do i = 1, n
            if (is_prime(i)) answer = answer + 1
        end do
    end function answer

end submodule euler_problem_0010_m

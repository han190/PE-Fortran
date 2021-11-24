submodule(interface_m) euler_problem_0010_m
    implicit none

contains

    module pure character(len=20) function euler0010()
        write (euler0010, "(i20)") answer()
    end function euler0010

    pure integer(i64) function answer()
        use prime_m, only: sieve_of_Sundaram
        implicit none

        integer(i32), parameter :: n = 2000000_i32
        logical, allocatable :: is_prime(:)
        integer(i64) :: i

        call sieve_of_Sundaram(n, is_prime)
        answer = sum([(i*2 + 1, i=1, size(is_prime))], is_prime) + 2
    end function answer

end submodule euler_problem_0010_m

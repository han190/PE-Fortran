submodule(interface_m) euler_problem_0003_m
    implicit none

contains

    module pure character(len=20) function euler0003()
        write (euler0003, "(i20)") answer()
    end function euler0003

    pure integer(i64) function answer()
        use prime_m, only: Sieve_of_Sundaram
        implicit none

        integer(i64), parameter :: n = 600851475143_i64
        logical, allocatable :: is_prime(:)
        integer(i32) :: i

        associate (upper_bound => int(sqrt(real(n, sp)) + 1._sp, i64))
            call Sieve_of_Sundaram(upper_bound, is_prime)
        end associate

        do i = size(is_prime), 1, -1
            if (is_prime(i) .and. mod(n, i*2 + 1_i64) == 0) then
                answer = i*2 + 1
                exit
            end if
        end do
    end function answer

end submodule euler_problem_0003_m

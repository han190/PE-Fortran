submodule(interface_m) euler_problem_0026_m
    implicit none

contains

    module character(len=20) function euler0026()
        write (euler0026, "(i20)") answer()
    end function euler0026

    pure integer(i64) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i64), parameter :: limit = 1000
        logical, allocatable :: is_prime(:)
        integer(i64) :: temp, k, i

        call Sieve_of_Eratosthenes(limit, is_prime)
        temp = 0; k = 1; answer = 0
        do i = 7, limit
            if (is_prime(i)) then
                temp = multiplicative_order(i, 10_i64)
                if (temp > k) then
                    k = temp
                    answer = i
                end if
            end if
        end do
    end function answer

    pure integer(i64) function multiplicative_order(n, m)
        integer(i64), intent(in) :: n, m
        integer(i64) :: k

        multiplicative_order = 1
        k = 1
        do
            associate (x => mod(k*m, n))
                if (x /= 1) then
                    k = x
                    multiplicative_order = multiplicative_order + 1
                else
                    exit
                end if
            end associate
        end do
    end function multiplicative_order

end submodule euler_problem_0026_m

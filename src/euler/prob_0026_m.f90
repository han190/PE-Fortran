submodule(euler_interface_m) euler_prob_0026_m
    implicit none

contains

    module character(len=20) function euler0026()
        write (euler0026, "(i20)") answer(1000_int64)
    end function euler0026

    function answer(n) result(ret)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none

        integer(int64), intent(in) :: n
        integer(int64) :: ret, tmp, k, i
        logical, allocatable :: is_prime(:)

        call sieve_of_Eratosthenes(n, is_prime)
        tmp = 0_int64; k = 1_int64; ret = 0_int64
        do i = 7_int64, n
            if (is_prime(i)) then
                tmp = multiplicative_order(i, 10_int64)
                if (tmp > k) then
                    k = tmp
                    ret = i
                end if
            end if
        end do
    end function answer

    function multiplicative_order(n, m) result(ret)
        integer(int64), intent(in) :: n, m
        integer(int64) :: ret, res, k

        k = 1_int64; ret = 1_int64
        do
            res = mod(k*m, n)
            if (res /= 1_int64) then
                k = res
                ret = ret + 1_int64
            else
                exit
            end if
        end do
    end function multiplicative_order

end submodule euler_prob_0026_m

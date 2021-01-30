submodule(euler_interface_m) euler_prob_0050_m
    implicit none

contains

    module character(len=20) function euler0050()
        write (euler0050, "(i20)") ans(1000000_int64)
    end function euler0050

    integer(int64) function ans(upper_lim)
        integer(int64), intent(in) :: upper_lim
        integer(int64) :: n, i, j, cur_sum
        integer(int64), allocatable :: p_arr(:)

        call prime_arr(upper_lim, p_arr)
        n = 0_int64

        do
            n = n + 1_int64
            if (sum(p_arr(1:n)) > upper_lim) exit
        end do

        n = n - 1_int64
        j = 1_int64
        cur_sum = sum(p_arr(1:n))

        outer: do i = n, 1, -1
            j = 1_int64
            cur_sum = sum(p_arr(1:i))

            inner: do
                if (any(cur_sum == p_arr)) exit outer
                cur_sum = cur_sum - p_arr(j)
                j = j + 1_int64
            end do inner
        end do outer

        ans = cur_sum
    end function ans

    subroutine prime_arr(n, primes)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none
        integer(int64), intent(in) :: n
        integer(int64), allocatable, intent(out) :: primes(:)
        integer(int64) :: i, j
        logical, allocatable :: is_prime(:)

        call sieve_of_Eratosthenes(n, is_prime)
        allocate (primes(count(is_prime)))

        j = 1
        do i = 2, n
            if (is_prime(i)) then
                primes(j) = i
                j = j + 1
            end if
        end do
    end subroutine prime_arr

end submodule euler_prob_0050_m

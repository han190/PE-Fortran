submodule(euler_interface_m) euler_prob_0058_m
    implicit none

contains

    module character(len=20) function euler0058()
        write (euler0058, "(i20)") ans()
    end function euler0058

    integer(int64) function ans()
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none

        logical, allocatable :: is_prime(:)
        integer(int64) :: n, k, arr(4)
        integer(int64), parameter :: rng = 700000000_int64

        call sieve_of_Eratosthenes(rng, is_prime)
        k = 0
        n = 2

        main_loop: do
            call diagonal_nums(n, arr)
            if (arr(3) >= rng) then
                error stop "Max range reached."
            end if
            k = k + count(is_prime(arr(1:3)))

            associate (p_nums => real(k), tot_nums => real((n - 1)*4 + 1))
                if (p_nums/tot_nums < 0.1_sp) exit main_loop
            end associate
            n = n + 1
        end do main_loop

        ans = side_len(n)
    end function ans

    subroutine diagonal_nums(n, arr)
        integer(int64), intent(in) :: n
        integer(int64), intent(out) :: arr(:)
        integer(int64) :: bottom_left, diff

        bottom_left = (n*2 - 1)**2
        diff = (n - 1)*2
        arr(1) = bottom_left - 3*diff
        arr(2) = bottom_left - 2*diff
        arr(3) = bottom_left - diff
        arr(4) = bottom_left
    end subroutine diagonal_nums

    integer(int64) function side_len(n)
        integer(int64), intent(in) :: n

        side_len = n*2_int64 - 1_int64
    end function side_len

end submodule euler_prob_0058_m

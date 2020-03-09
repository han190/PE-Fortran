submodule(euler_interface_m) euler_prob_0046_m
    implicit none

contains

    module character(len=20) function euler0046()
        write (euler0046, "(i20)") ans(10000)
    end function euler0046

    integer function ans(n)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none
        integer, intent(in) :: n
        integer :: j, res
        logical :: not_found
        logical, allocatable :: is_prime(:)

        call sieve_of_Eratosthenes(n, is_prime)
        res = 1; not_found = .true.

        outer: do while (not_found)
            res = res + 2
            j = 2
            not_found = .false.

            inner: do while (res >= j)
                if (                                                           &
                    is_prime(j) .and.                                          &
                    is_twice_square(res - j)                                   &
                ) then
                    not_found = .true.
                    exit inner
                end if
                j = j + 1
            end do inner
        end do outer

        ans = res
    end function ans

    logical function is_twice_square(n)
        integer, intent(in) :: n
        real(dp) :: sqrt_nover2

        is_twice_square = .false.
        sqrt_nover2 = sqrt( 0.5_dp * real(n, dp) )

        if (                                                                   &
            sqrt_nover2 -                                                      &
            real( floor(sqrt_nover2), dp ) <                                   &
            tiny_dp                                                            &
        ) is_twice_square = .true.
    end function is_twice_square

end submodule euler_prob_0046_m

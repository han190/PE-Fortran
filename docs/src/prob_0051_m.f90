submodule(euler_interface_m) euler_prob_0051_m
    implicit none

    ! Nicely done by my two little wheels, int_2_arr and arr_2_int!
    !---------------------------------------------------------------------------

contains

    module character(len=20) function euler0051()
        write (euler0051, "(i20)") answer()
    end function euler0051

    function answer() result(ret)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none

        integer, parameter :: PRIME_START = 100000, PRIME_END = 999999
        integer :: i, j, s, idx, tmp, p_knt, c_knt, ret
        integer, allocatable :: prime(:), arr(:)
        logical, allocatable :: is_prime(:)

        call sieve_of_Eratosthenes(PRIME_END, is_prime)
        prime = pack([(i, i=0, PRIME_END)], is_prime)
        prime = pack(prime, prime > PRIME_START)

        outer: do idx = 1, size(prime)
            call int_2_arr(prime(idx), arr)
            associate (k => [(count(arr == i), i=0, 2)])
                j = findloc([(mod(k(i), 3) == 0, i=1, 3)], value=.true., dim=1)
                if (j == 0) cycle outer
            end associate

            p_knt = 0; c_knt = 0
            associate (r => pack([(i, i=1, size(arr))], arr == arr(j)))
                s = 0; if (any(r == 1)) s = 1
                inner: do j = s, 9
                    arr(r) = j
                    call arr_2_int(arr, tmp)
                    if (is_prime(tmp)) then
                        p_knt = p_knt + 1
                    else
                        c_knt = c_knt + 1
                    end if
                    if (p_knt >= 8) exit outer
                    if (c_knt >= 3) exit inner
                end do inner
            end associate
        end do outer
        ret = prime(idx)
    end function answer

end submodule euler_prob_0051_m

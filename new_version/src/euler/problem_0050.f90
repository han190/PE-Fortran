submodule(interface_m) euler_problem_0050_m
    implicit none

contains

    module character(len=20) function euler0050()
        write (euler0050, "(i20)") answer()
    end function euler0050

    pure integer(i64) function answer()
        use prime_m, only: get_primes
        implicit none

        integer(i64), parameter :: upper_ = 1000000
        integer(i64) :: n, i, j

        associate (primes => get_primes(upper_, "Sieve of Eratosthenes"))
            n = 0
            do while (sum(primes(1:n)) <= upper_)
                n = n + 1
            end do

            outer: do i = n - 1, 1, -1
                answer = sum(primes(1:i)); j = 1
                inner: do
                    if (any(answer == primes)) exit outer
                    answer = answer - primes(j)
                    j = j + 1
                end do inner
            end do outer
        end associate
    end function answer

end submodule euler_problem_0050_m

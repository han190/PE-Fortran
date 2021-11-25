submodule(interface_m) euler_problem_0012_m
    implicit none

contains

    module pure character(len=20) function euler0012()
        write (euler0012, "(i20)") answer()
    end function euler0012

    pure integer(i32) function answer()
        use prime_m, only: Sieve_of_Sundaram
        implicit none

        integer(i32), parameter :: n = 500
        integer(i32), parameter :: limit = 20
        integer(i32), allocatable :: primes(:)
        logical, allocatable :: is_prime(:)
        integer(i32) :: i

        call Sieve_of_Sundaram(limit, is_prime)
        primes = [2, pack([(i*2 + 1, i=1, size(is_prime))], is_prime)]

        i = 0
        answer = 0
        do
            i = i + 1
            answer = answer + i
            if (number_of_divisors(answer, primes) > n) exit
        end do
    end function answer

    !> Count number of divisors.
    !> Write an integer in a form of n = p1**e1 + p1**e2 + ...
    !> where p_i are the primes and e_i are the number of existences.
    !> Then, number of divisors is (e1 + 1)*(e2 + 1)*...
    pure function number_of_divisors(n, primes) result(ret)
        integer(i32), intent(in) :: n, primes(:)
        integer(i32) :: ret
        integer(i32) :: temp, e, i

        temp = n
        ret = 1
        outer: do i = 1, size(primes)
            e = 1
            inner: do
                if (mod(temp, primes(i)) /= 0) then
                    exit inner
                else if (temp == 1) then
                    exit outer
                end if

                e = e + 1
                temp = temp/primes(i)
            end do inner
            ret = ret*e
        end do outer
    end function number_of_divisors

end submodule euler_problem_0012_m

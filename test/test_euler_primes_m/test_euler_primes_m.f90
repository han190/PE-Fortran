program main

    use euler_primes_m, only: sieve_of_Eratosthenes
    implicit none

    generate_arr_of_primes: block
        integer :: i
        logical, allocatable :: is_prime(:)
        integer, allocatable :: prime(:)
        integer, parameter :: PRIME_UPPER_BOUND = 100

        call sieve_of_Eratosthenes(PRIME_UPPER_BOUND, is_prime)
        prime = pack([(i, i = 0, PRIME_UPPER_BOUND - 1)], is_prime)
        print "(a)", new_line("a")//"Test sieve_of_Eratosthenes"
        print "(a, i5)", "Print primes less than: ", PRIME_UPPER_BOUND
        print "(10(i3, ','))", prime
    end block generate_arr_of_primes

end program main
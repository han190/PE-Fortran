program main

    use utility_m
    use prime_m, only: Sieve_of_Sundaram
    implicit none

    logical, allocatable :: is_prime(:)
    integer, parameter :: upper_bound = 100
    integer, allocatable :: primes(:)
    integer :: i

    call Sieve_of_Sundaram(upper_bound, is_prime)
    primes = [2, pack([(i*2 + 1, i=1, size(is_prime))], is_prime)]

end program main
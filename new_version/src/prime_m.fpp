#:set integer_kinds = ['i32', 'i64']

module prime_m

    use constant_m
    use utility_m, only: sqrt
    implicit none
    private

    public :: is_prime
    public :: Sieve_of_Eratosthenes

    !> A generic interface that tells if an integer is prime.
    interface is_prime
        #: for T in integer_kinds
        module procedure is_prime_${T}$
        #: endfor
    end interface is_prime

    !> An algorithm for finding all prime numbers within a limit.
    interface Sieve_of_Eratosthenes
        #: for T in integer_kinds
        module procedure Sieve_of_Eratosthenes_logical_${T}$
        module procedure Sieve_of_Eratosthenes_${T}$
        #: endfor
    end interface Sieve_of_Eratosthenes

contains

    #: for T in integer_kinds
    !> A generic interface that tells if an integer is prime.
    elemental logical function is_prime_${T}$ (n)
        integer(${T}$), intent(in) :: n
        integer(${T}$) :: limit, i

        is_prime_${T}$ = .false.
        limit = int(sqrt(real(n, sp)) + 1_${T}$)
        if (n <= 1_${T}$) then
            is_prime_${T}$ = .false.
        else if (n <= 3_${T}$) then
            is_prime_${T}$ = .true.
        else if (mod(n, 2_${T}$) == 0_${T}$) then
            is_prime_${T}$ = .false.
        else
            loop_1: do i = 3_${T}$, limit, 2_${T}$
                if (mod(n, i) == 0_${T}$) then
                    is_prime_${T}$ = .false.
                    exit loop_1
                else
                    is_prime_${T}$ = .true.
                end if
            end do loop_1
        end if
    end function is_prime_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Sieve of Eratosthenes for kind: ${T}$.
    pure subroutine Sieve_of_Eratosthenes_logical_${T}$ (n, primes)
        integer(${T}$), intent(in) :: n
        logical, intent(out) :: primes(:)
        integer(${T}$) :: i

        primes(1:2) = [.false., .true.]
        primes(3:n:2) = .true.
        primes(4:n:2) = .false.

        do i = 2, sqrt(n)
            if (primes(i)) primes(i*i:n:i) = .false.
        end do
    end subroutine Sieve_of_Eratosthenes_logical_${T}$

    #: endfor

    #: for T in integer_kinds
    !> Sieve of Eratosthenes for kind: ${T}$.
    pure subroutine Sieve_of_Eratosthenes_${T}$ (n, primes)
        integer(${T}$), intent(in) :: n
        integer(${T}$), allocatable, intent(out) :: primes(:)
        logical, allocatable :: prime_l(:)
        integer(${T}$) :: i

        allocate (prime_l(n))
        call Sieve_of_Eratosthenes_logical_${T}$ (n, prime_l)
        primes = pack([(i, i=1, n)], prime_l)
    end subroutine Sieve_of_Eratosthenes_${T}$

    #: endfor

end module prime_m

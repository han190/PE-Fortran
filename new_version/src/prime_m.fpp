module prime_m

    use constant_m
    implicit none
    private

    public :: is_prime
    public :: Sieve_of_Eratosthenes
    public :: Sieve_of_Sundaram
    public :: get_primes

    #:set integer_kinds = ['i32', 'i64']

    !> A generic interface that tells if an integer is prime.
    interface is_prime
        #: for T in integer_kinds
        module procedure is_prime_${T}$
        #: endfor
    end interface is_prime

    !> An algorithm for finding all prime numbers within a limit.
    interface Sieve_of_Eratosthenes
        #: for T in integer_kinds
        module procedure Sieve_of_Eratosthenes_${T}$
        #: endfor
    end interface Sieve_of_Eratosthenes

    !> An algorithm for finding all odd primes within a limit.
    interface Sieve_of_Sundaram
        #: for T in integer_kinds
        module procedure Sieve_of_Sundaram_${T}$
        #: endfor
    end interface Sieve_of_Sundaram

    !> Wrappers
    interface get_primes
        #: for T in integer_kinds
        module procedure get_primes_${T}$
        #: endfor
    end interface get_primes

contains

    !> A generic interface that tells if an integer is prime.
    #: for T in integer_kinds
    pure logical function is_prime_${T}$ (n)
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

    !> Sieve of Eratosthenes
    #: for T in integer_kinds
    pure subroutine Sieve_of_Eratosthenes_${T}$ (n, prime_arr)
        integer(${T}$), intent(in) :: n
        logical, allocatable, intent(out) :: prime_arr(:)
        integer(${T}$) :: i

        allocate (prime_arr(n))
        prime_arr = .true.
        prime_arr(1) = .false.
        do i = 2, floor(sqrt(real(n, dp)))
            if (prime_arr(i)) prime_arr(i*i:n:i) = .false.
        end do
    end subroutine Sieve_of_Eratosthenes_${T}$
    #: endfor

    !> Sieve of Sundaram
    #: for T in integer_kinds
    pure subroutine Sieve_of_Sundaram_${T}$ (n, primes)
        integer(${T}$), intent(in) :: n
        logical, allocatable, intent(out) :: primes(:)
        integer(${T}$) :: i

        associate (k => (n - 3)/2 + 1)
            allocate (primes(k))
            primes = .true.

            do i = 0, (int(sqrt(real(n))) - 3)/2
                associate (p => 2*i + 3)
                    primes((p*p - 3)/2 + 1:k:p) = .false.
                end associate
            end do
        end associate
    end subroutine Sieve_of_Sundaram_${T}$
    #: endfor

    !> Wrapper
    #: for T in integer_kinds
    pure function get_primes_${T}$ (n, algorithm) result(primes)
        integer(${T}$), intent(in) :: n
        integer(${T}$), allocatable :: primes(:)
        character(*), intent(in) :: algorithm
        logical, allocatable :: primes_l(:)
        integer(${T}$) :: i

        select case (algorithm)
        case ("Sieve of Sundaram")
            call Sieve_of_Sundaram_${T}$ (n, primes_l)
            primes = [2_${T}$, pack([(i*2 + 1, i=1, size(primes_l))], primes_l)]
        case ("Sieve of Eratosthenes")
            call Sieve_of_Eratosthenes_${T}$ (n, primes_l)
            primes = pack([(i, i=1, size(primes_l))], primes_l)
        case default
            error stop "Invalid options."
        end select
    end function get_primes_${T}$
    #: endfor

end module prime_m

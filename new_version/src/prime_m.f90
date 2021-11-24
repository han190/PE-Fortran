module prime_m

    use constant_m
    implicit none
    private

    public :: is_prime, sieve_of_Eratosthenes, sieve_of_Sundaram

    !> A generic interface that tells if an integer is prime.
    interface is_prime
        module procedure is_prime_i32, is_prime_i64
    end interface is_prime

    !> An algorithm for finding all prime numbers within a limit.
    interface sieve_of_Eratosthenes
        module procedure sieve_of_Eratosthenes_i32
        module procedure sieve_of_Eratosthenes_i64
    end interface sieve_of_Eratosthenes

    interface sieve_of_Sundaram
        module procedure sieve_of_Sundaram_i32
    end interface sieve_of_Sundaram

contains

    !>  for 32 bit integer.
    pure logical function is_prime_i32(n)
        integer, intent(in) :: n
        integer :: limit, i

        is_prime_i32 = .false.
        limit = int(sqrt(real(n, sp)) + 1)
        if (n <= 1) then
            is_prime_i32 = .false.
        else if (n <= 3) then
            is_prime_i32 = .true.
        else if (mod(n, 2) == 0) then
            is_prime_i32 = .false.
        else
            loop_1: do i = 3, limit, 2
                if (mod(n, i) == 0) then
                    is_prime_i32 = .false.
                    exit loop_1
                else
                    is_prime_i32 = .true.
                end if
            end do loop_1
        end if
    end function is_prime_i32

    !>  for 64 bit integer.
    pure logical function is_prime_i64(n)
        integer(i64), intent(in) :: n
        integer(i64) :: limit, i

        is_prime_i64 = .false.
        limit = int(sqrt(real(n, sp)) + 1_i64)
        if (n <= 1_i64) then
            is_prime_i64 = .false.
        else if (n <= 3_i64) then
            is_prime_i64 = .true.
        else if (mod(n, 2_i64) == 0_i64) then
            is_prime_i64 = .false.
        else
            loop_1: do i = 3_i64, limit, 2_i64
                if (mod(n, i) == 0_i64) then
                    is_prime_i64 = .false.
                    exit loop_1
                else
                    is_prime_i64 = .true.
                end if
            end do loop_1
        end if
    end function is_prime_i64

    !> sieve of Eratosthenes for 32 bit integer.
    pure subroutine sieve_of_Eratosthenes_i32(n, prime_arr)
        integer, intent(in) :: n
        logical, allocatable, intent(out) :: prime_arr(:)
        integer :: i, j

        allocate (prime_arr(n))
        prime_arr = .true.
        prime_arr(1) = .false.
        do i = 2, floor(sqrt(real(n, dp)))
            if (prime_arr(i)) prime_arr(i*i:n:i) = .false.
        end do
    end subroutine sieve_of_Eratosthenes_i32

    !> sieve of Eratosthenes for 64 bit integer.
    pure subroutine sieve_of_Eratosthenes_i64(n, prime_arr)
        integer(i64), intent(in) :: n
        logical, allocatable, intent(out) :: prime_arr(:)
        integer(i64) :: i, j

        allocate (prime_arr(n))
        prime_arr = .true.
        prime_arr(1) = .false.
        do i = 2_i64, floor(sqrt(real(n, dp)), i64)
            if (prime_arr(i)) prime_arr(i*i:n:i) = .false.
        end do
    end subroutine sieve_of_Eratosthenes_i64

    pure subroutine sieve_of_Sundaram_i32(n, primes)
        integer(i32), intent(in) :: n
        logical, allocatable, intent(out) :: primes(:)
        integer(i32) :: i, j, k

        k = (n - 3)/2 + 1
        allocate(primes(k)); primes = .true.

        do i = 0, (int(sqrt(real(n))) - 3)/2
            associate (p => 2*i + 3)
                primes((p*p - 3)/2 + 1:k:p) = .false.
            end associate
        end do
    end subroutine sieve_of_Sundaram_i32

end module prime_m

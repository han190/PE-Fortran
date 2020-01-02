module euler_primes_m 
    use iso_fortran_env, only: int64
    use euler_utils_m, only: sp, dp 
    implicit none 
    private

    public :: is_prime
    interface is_prime 
        module procedure is_prime_int32, is_prime_int64 
    end interface is_prime 

    public :: sieve_of_Eratosthenes
    interface sieve_of_Eratosthenes 
        module procedure sieve_of_Eratosthenes_int32
        module procedure sieve_of_Eratosthenes_int64 
    end interface sieve_of_Eratosthenes

contains 

    logical function is_prime_int32(n)
        integer, intent(in) :: n 
        integer :: limit, i  

        is_prime_int32 = .false.
        limit = int( sqrt( real(n, sp) ) + 1 )
        if ( n <= 1 ) then
            is_prime_int32 = .false.
        else if ( n <= 3 ) then
            is_prime_int32 = .true.
        else if ( mod(n, 2) == 0 ) then
            is_prime_int32 = .false.
        else
            loop_1: do i = 3, limit, 2
                if ( mod(n, i) == 0 ) then
                    is_prime_int32 = .false.
                    exit loop_1
                else
                    is_prime_int32 = .true.
                end if
            end do loop_1
        end if
    end function is_prime_int32 

    logical function is_prime_int64(n)
        integer(int64), intent(in) :: n 
        integer(int64) :: limit, i  

        is_prime_int64 = .false.
        limit = int( sqrt( real(n, sp) ) + 1_int64 )
        if ( n <= 1_int64 ) then
            is_prime_int64 = .false.
        else if ( n <= 3_int64 ) then
            is_prime_int64 = .true.
        else if ( mod(n, 2_int64) == 0_int64 ) then
            is_prime_int64 = .false.
        else
            loop_1: do i = 3_int64, limit, 2_int64
                if ( mod(n, i) == 0_int64 ) then
                    is_prime_int64 = .false.
                    exit loop_1
                else
                    is_prime_int64 = .true.
                end if
            end do loop_1
        end if
    end function is_prime_int64 

    subroutine sieve_of_Eratosthenes_int32(n, is_prime)
        integer, intent(in) :: n 
        logical, allocatable :: is_prime(:)
        integer :: i, j 

        allocate( is_prime(0:n) )
        is_prime = .true. 
        is_prime(0:1) = .false. 
        do i = 2, floor( sqrt( real(n, dp) ) )
            if ( is_prime(i) ) then
                j = i * i
                do while ( j <= n )
                    is_prime(j) = .false.
                    j = j + i
                end do
            end if
        end do
    end subroutine sieve_of_Eratosthenes_int32

    subroutine sieve_of_Eratosthenes_int64(n, is_prime)
        integer(int64), intent(in) :: n 
        logical, allocatable :: is_prime(:)
        integer(int64) :: i, j 

        allocate( is_prime(0:n) )
        is_prime = .true. 
        is_prime(0:1) = .false. 
        do i = 2_int64, floor( sqrt( real(n, dp) ), int64 )
            if ( is_prime(i) ) then
                j = i * i
                do while ( j <= n )
                    is_prime(j) = .false.
                    j = j + i
                end do
            end if
        end do
    end subroutine sieve_of_Eratosthenes_int64

end module euler_primes_m
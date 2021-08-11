program main

    use euler_primes_m, only: sieve_of_Eratosthenes
    implicit none

    call title("'euler_primes_m'")
    generate_arr_of_primes: block
        integer :: i
        logical, allocatable :: is_prime(:)
        integer, allocatable :: prime(:)
        integer, parameter :: PRIME_UPPER_BOUND = 100
        integer, allocatable :: primes_under_100(:)
        logical :: cond

        primes_under_100 = [2,  3,  5,  7, 11, 13, 17, 19, 23, &
                            29, 31, 37, 41, 43, 47, 53, 59, 61, &
                            67, 71, 73, 79, 83, 89, 97]
        call sieve_of_Eratosthenes(PRIME_UPPER_BOUND, is_prime)
        prime = pack([(i, i=0, PRIME_UPPER_BOUND)], is_prime)
        cond = all(primes_under_100 == prime)
        call test(cond, "sieve_of_Eratosthenes")
    end block generate_arr_of_primes

contains

    subroutine test(input_condition, input_name)
        logical, intent(inout) :: input_condition
        character(*), intent(in) :: input_name

        if (input_condition) then
            print "(a)", "Testing "//input_name//"... passed."
        else
            print "(a)", "Testing "//input_name//"... not passed!"
            stop
        end if
    end subroutine test

    subroutine title(input_name)
        character(*), intent(in) :: input_name

        print "(a)", "Testing "//input_name//"..."
    end subroutine title

end program main

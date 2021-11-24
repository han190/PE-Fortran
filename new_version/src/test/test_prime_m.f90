program main

    use prime_m, only: Sieve_of_Sundaram, Sieve_of_Eratosthenes
    implicit none

    call title("'primes_m'")
    generate_arr_of_primes: block
        integer :: i
        logical, allocatable :: is_prime(:)
        integer, parameter :: upper_bound = 100
        integer, allocatable :: prime(:), primes_under_100(:)
        logical :: conditions

        primes_under_100 = [2, 3, 5, 7, 11, 13, 17, 19, 23, &
                            29, 31, 37, 41, 43, 47, 53, 59, &
                            61, 67, 71, 73, 79, 83, 89, 97]
        call Sieve_of_Sundaram(upper_bound, is_prime)
        prime = [2, pack([(i*2 + 1, i=1, size(is_prime))], is_prime)]
        conditions = all(primes_under_100 == prime)
        call test(conditions, "Sieve_of_Sundaram")

        deallocate (prime, is_prime)
        call Sieve_of_Eratosthenes(upper_bound, is_prime)
        prime = pack([(i, i=1, size(is_prime))], is_prime)
        conditions = all(primes_under_100 == prime)
        call test(conditions, "Sieve_of_Eratosthenes")
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

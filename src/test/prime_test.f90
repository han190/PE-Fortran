program main

  use prime_module, only: is_prime, Sieve_of_Eratosthenes
  implicit none

  call title("'primes_module'")
  generate_arr_of_primes: block
    integer, parameter :: n = 100
    integer, allocatable :: primes(:), primes_100(:)
    integer :: i

    primes_100 = pack([(i, i=1, n)], [(is_prime(i), i=1, n)])
    call Sieve_of_Eratosthenes(n, primes)
    call test(all(primes_100 == primes), "Sieve_of_Eratosthenes")
  end block generate_arr_of_primes

contains

  subroutine test(input_condition, input_name)
    logical, intent(in) :: input_condition
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

    print "(a)", "Testing "//input_name
  end subroutine title

end program main

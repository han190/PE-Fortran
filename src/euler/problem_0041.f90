submodule(interface_module) euler_problem_0041_submodule
  implicit none

contains

  module character(len=20) function euler0041()
    write (euler0041, "(i20)") answer()
  end function euler0041

  elemental integer(i32) function answer()
    use prime_module, only: Sieve_of_Eratosthenes
    implicit none

    integer(i32), parameter :: n = 7654321
    logical, allocatable :: is_prime(:)

    allocate (is_prime(n))
    call Sieve_of_Eratosthenes(n, is_prime)
    do answer = n, 1, -1
      if (is_prime(answer) .and. is_pandigital(answer)) exit
    end do
  end function answer

end submodule euler_problem_0041_submodule

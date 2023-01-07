submodule(interface_module) euler_problem_0049_submodule
implicit none

contains

module character(len=20) function euler0049()
  write (euler0049, "(a20)") trim(answer())
end function euler0049

pure character(len=12) function answer()
  use prime_module, only: Sieve_of_Eratosthenes
  implicit none

  integer(i32), parameter :: n = 10000
  integer(i32) :: i
  logical, allocatable :: is_prime(:)
  integer(i32), parameter :: by = 3330

  allocate (is_prime(n))
  call Sieve_of_Eratosthenes(n, is_prime)
  do i = 9973, 7661, -1
    associate (a => i, b => i - by, c => i - by*2)
      if (.not. all([is_prime(a), is_prime(b), is_prime(c)])) cycle
      if (sort_(a) == sort_(b) .and. sort_(a) == sort_(c)) exit
    end associate
  end do

  write (answer, "(3(i4))") i - by*2, i - by, i
end function answer

pure integer(i32) function sort_(n)
  use stdlib_sorting, only: sort
  implicit none

  integer(i32), intent(in) :: n
  integer(i32), allocatable :: array(:)

  array = to_array(n)
  call sort(array)
  sort_ = to_integer(array)
end function sort_

end submodule euler_problem_0049_submodule

submodule(module_interface) submodule_euler0035
implicit none
contains

module subroutine euler0035(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32), parameter :: n = 1000000
  type(sieve_type(len=n)) :: sieve
  logical, allocatable :: is_circular(:)
  integer(int32), allocatable :: array(:)
  logical, pointer :: check(:) => null()
  integer(int32) :: i

  call sift(sieve, check=check)
  allocate (is_circular(n))
  is_circular = .false.

  do i = 100, n
    if (is_circular(i)) cycle
    call is_circular_prime(i, check, array)
    if (allocated(array)) is_circular(array) = .true.
  end do
  write (problem%answer, "(i20)") count(is_circular) + 13
  nullify (check)
end subroutine euler0035

pure subroutine is_circular_prime(n, are_primes, array)
  integer(int32), intent(in) :: n
  logical, intent(in) :: are_primes(:)
  integer(int32), allocatable, intent(out) :: array(:)
  integer(int32) :: tmp, i

  if (allocated(array)) deallocate (array)
  associate (x => num_digits(n))
    tmp = n
    do i = 1, x
      if (.not. are_primes(tmp)) return
      tmp = rotate(tmp)
    end do

    allocate (array(x))
    call circular_array(n, array)
  end associate
end subroutine is_circular_prime

pure subroutine circular_array(n, array)
  integer(int32), intent(in) :: n
  integer(int32), intent(out) :: array(:)
  integer(int32) :: i, tmp

  tmp = n
  do i = 1, num_digits(n)
    array(i) = tmp
    tmp = rotate(tmp)
  end do
end subroutine circular_array

elemental integer(int32) function rotate(n)
  integer(int32), intent(in) :: n

  rotate = unit_digit(n)*10**(num_digits(n) - 1) + n/10
end function rotate

end submodule submodule_euler0035

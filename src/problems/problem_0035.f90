submodule(module_problem) submodule_euler0035
implicit none
contains

module subroutine euler0035(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: n = 1000000
  logical, allocatable :: check(:), is_circular(:)
  integer(int64), allocatable :: array(:)
  integer(int64) :: i

  check = sift(n)
  allocate (is_circular(n))
  is_circular = .false.
  do i = 100, n
    if (is_circular(i)) cycle
    call is_circular_prime(i, check, array)
    if (allocated(array)) is_circular(array) = .true.
  end do
  write (answer, "(i20)") count(is_circular) + 13
end subroutine euler0035

pure subroutine is_circular_prime(n, are_primes, array)
  integer(int64), intent(in) :: n
  logical, intent(in) :: are_primes(:)
  integer(int64), allocatable, intent(out) :: array(:)
  integer(int64) :: tmp, i

  associate (x => num_digits(n))
    tmp = n
    do i = 1, x
      if (.not. are_primes(tmp)) return
      tmp = rotate(tmp)
    end do

    call re_allocate(array, x)
    call circular_array(n, array)
  end associate
end subroutine is_circular_prime

pure subroutine circular_array(n, array)
  integer(int64), intent(in) :: n
  integer(int64), intent(out) :: array(:)
  integer(int64) :: i, tmp

  tmp = n
  do i = 1, num_digits(n)
    array(i) = tmp
    tmp = rotate(tmp)
  end do
end subroutine circular_array

elemental integer(int64) function rotate(n)
  integer(int64), intent(in) :: n

  rotate = unit_digit(n)*10**(num_digits(n) - 1) + n/10
end function rotate

end submodule submodule_euler0035

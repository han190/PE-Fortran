submodule(module_interface) submodule_euler0058
implicit none
contains

module subroutine euler0058(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: i, j, n

  j = 0
  n = 2
  do
    associate (array => diagonal(n))
      do i = 1, 3
        if (is_prime(array(i))) j = j + 1
      end do
    end associate

    associate (primes => real(j), total => real((n - 1)*4 + 1))
      if (primes/total < 0.1_sp) exit
    end associate
    n = n + 1
  end do
  write (problem%answer, "(i20)") side_length(n)
end subroutine euler0058

pure function diagonal(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64), allocatable :: ret(:)
  integer(int64) :: i

  ret = (n*2 - 1)**2 - [(i*2*(n - 1), i=3, 0, -1)]
end function diagonal

elemental integer(int64) function side_length(n)
  integer(int64), intent(in) :: n

  side_length = n*2 - 1
end function side_length

end submodule submodule_euler0058

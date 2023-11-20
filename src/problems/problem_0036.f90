submodule(module_problem) submodule_euler0036
implicit none
contains

module subroutine euler0036(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 1000000
  integer(int64) :: i, sln

  sln = 0
  do i = 1, n
    if (are_palindromics(i)) sln = sln + i
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0036

pure logical function are_palindromics(n)
  integer(int64), intent(in) :: n

  if (.not. is_palindromic(n)) then
    are_palindromics = .false.
    return
  end if

  associate (x => to_binary(n))
    are_palindromics = all(x == x(size(x):1:-1))
  end associate
end function are_palindromics

pure function to_binary(n) result(ret)
  integer(int64), intent(in) :: n
  integer(int64), allocatable :: ret(:)
  integer(int64) :: i, tmp

  associate (length => floor(log2(n)) + 1)
    allocate (ret(length))
    tmp = n
    do i = 1, length
      ret(i) = mod(tmp, 2_int64)
      tmp = tmp/2_int64
    end do
  end associate
end function to_binary

elemental real(real64) function log2(n)
  integer(int64), intent(in) :: n

  log2 = log(real(n, real64))/log(2.0)
end function log2

end submodule submodule_euler0036

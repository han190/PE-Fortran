submodule(module_interface) submodule_euler0043
implicit none
integer(int64), parameter :: primes(7) = [17, 13, 11, 7, 5, 3, 2]
contains

module subroutine euler0043(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: i, sln, tmp, array(10)
  type(permutation_type(n=10)) :: permutation

  call initialize(permutation)
  sln = 0
  do while (permutable(permutation))
    array = index(permutation) - 1
    tmp = to_integer(array)
    if (divisible(tmp)) sln = sln + tmp
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0043

pure logical function divisible(n)
  integer(int64), intent(in) :: n
  integer(int64) :: tmp, i, x

  tmp = n
  do i = 1, 7
    x = mod(tmp, 1000_int64)
    divisible = mod(x, primes(i)) == 0
    if (.not. divisible) exit
    tmp = tmp/10
  end do
end function divisible

end submodule submodule_euler0043

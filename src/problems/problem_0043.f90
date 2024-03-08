submodule(module_problem) submodule_euler0043
implicit none
integer(int64), parameter :: primes(7) = [17, 13, 11, 7, 5, 3, 2]
contains

module subroutine euler0043(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64) :: sln, tmp
  type(permutation_type) :: permutation

  permutation = new_permutation(n=10_int64)
  sln = 0
  do while (permutable(permutation))
    tmp = to_integer(index(permutation) - 1)
    if (divisible(tmp)) sln = sln + tmp
  end do
  write (answer, "(i20)") sln
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

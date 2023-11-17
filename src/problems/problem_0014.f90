submodule(module_problem) module_euler0014
implicit none
contains

module subroutine euler0014(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), allocatable :: array(:)
  integer(int64) :: len_, len_max, i, sln
  integer(int64), parameter :: istart = 1
  integer(int64), parameter :: iend = 10**6

  array = [(-1_int64, i=istart, iend)]
  len_max = 0
  sln = 0

  do i = istart, iend
    if (array(i) == -1) then
      len_ = len_collatz(i)
      array(i) = len_
    else
      len_ = array(i)
    end if

    if (len_ > len_max) then
      len_max = len_
      sln = i
    end if
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0014

elemental integer(int64) function len_collatz(n)
  integer(int64), intent(in) :: n
  integer(int64) :: tmp

  len_collatz = 0
  tmp = n
  do while (tmp /= 1)
    if (mod(tmp, 2_int64) == 0) then
      tmp = tmp/2
      len_collatz = len_collatz + 1
    else
      tmp = (tmp*3 + 1)/2
      len_collatz = len_collatz + 2
    end if
  end do
end function len_collatz

end submodule module_euler0014
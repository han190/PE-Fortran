submodule(module_problem) submodule_euler0062
implicit none
contains

module subroutine euler0062(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 10000
  integer(int64) :: i, x, counts(n)
  character(len=20) :: list(n)
  integer(int64), allocatable :: array(:)
  
  list = ""
  counts = 1
  do i = 1, n
    array = to_array(i**3)
    call quicksort(array, 1_int64, size(array, kind=int64))
    write (list(i), "(*(i1))") array
    x = findloc(list(:i - 1), value=list(i), dim=1)
    if (x /= 0) then
      counts(x) = counts(x) + 1
      if (counts(x) == 5) exit
    end if
  end do
  write (problem%answer, "(i20)") x**3
end subroutine euler0062

end submodule submodule_euler0062
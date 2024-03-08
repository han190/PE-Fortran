submodule(module_problem) submodule_euler0049
implicit none
contains

module subroutine euler0049(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: by = 3330
  integer(int64) :: i
  logical, allocatable :: check(:)

  check = sift(10000_int64)
  do i = 9973, 7661, -1
    associate (a => i, b => i - by, c => i - by*2)
      if (.not. all([check(a), check(b), check(c)])) cycle
      if (sort(a) == sort(b) .and. sort(a) == sort(c)) exit
    end associate
  end do
  write (answer, "(3(i4))") i - by*2, i - by, i
end subroutine euler0049

pure integer(int64) function sort(n)
  integer(int64), intent(in) :: n
  integer(int64), allocatable :: array(:)

  array = to_array(n)
  call quicksort(array, 1_int64, size(array, kind=int64))
  sort = to_integer(array)
end function sort

end submodule submodule_euler0049

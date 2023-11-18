submodule(module_problem) submodule_euler0049
implicit none
contains

module subroutine euler0049(problem)
  class(problem_type), intent(inout) :: problem
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
  write (problem%answer, "(3(i4))") i - by*2, i - by, i
end subroutine euler0049

pure integer(int64) function sort(n)
  integer(int64), intent(in) :: n
  integer(int64), allocatable :: array(:)

  array = to_array(n)
  call quicksort(array, 1_int64, size(array, kind=int64))
  sort = to_integer(array)
end function sort

pure recursive subroutine quicksort(arr, low, high)
  integer(int64), intent(inout) :: arr(:)
  integer(int64), intent(in) :: low, high
  integer(int64) :: pivot_loc

  if (low < high) then
    call partition(arr, low, high, pivot_loc)
    call quicksort(arr, low, pivot_loc - 1)
    call quicksort(arr, pivot_loc + 1, high)
  end if
end subroutine quicksort

pure subroutine partition(arr, low, high, pivot_loc)
  integer(int64), intent(inout) :: arr(:)
  integer(int64), intent(in) :: low, high
  integer(int64), intent(out) :: pivot_loc
  integer(int64) :: i, j, pivot

  pivot = arr(high)
  i = low - 1
  do j = low, high
    if (arr(j) < pivot) then
      i = i + 1
      call swap(arr(i), arr(j))
    end if
  end do
  call swap(arr(i + 1), arr(high))
  pivot_loc = i + 1
end subroutine partition

end submodule submodule_euler0049

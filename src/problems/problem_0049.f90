submodule(module_interface) submodule_euler0049
implicit none
contains

module subroutine euler0049(problem)
  type(problem_type), intent(inout) :: problem
  type(sieve_type(len=10000)) :: sieve
  integer(int32), parameter :: by = 3330
  integer(int32) :: i
  logical, pointer :: check(:) => null()

  call sift(sieve, check=check)
  do i = 9973, 7661, -1
    associate (a => i, b => i - by, c => i - by*2)
      if (.not. all([check(a), check(b), check(c)])) cycle
      if (sort(a) == sort(b) .and. sort(a) == sort(c)) exit
    end associate
  end do
  write (problem%answer, "(3(i4))") i - by*2, i - by, i
  nullify (check)
end subroutine euler0049

pure integer(int32) function sort(n)
  integer(int32), intent(in) :: n
  integer(int32), allocatable :: array(:)

  array = to_array(n)
  call quicksort(array, 1, size(array))
  sort = to_integer(array)
end function sort

pure recursive subroutine quicksort(arr, low, high)
  integer(int32), intent(inout) :: arr(:)
  integer, intent(in) :: low, high
  integer :: pivot_loc

  if (low < high) then
    call partition(arr, low, high, pivot_loc)
    call quicksort(arr, low, pivot_loc - 1)
    call quicksort(arr, pivot_loc + 1, high)
  end if
end subroutine quicksort

pure subroutine partition(arr, low, high, pivot_loc)
  integer(int32), intent(inout) :: arr(:)
  integer(int32), intent(in) :: low, high
  integer(int32), intent(out) :: pivot_loc
  integer(int32) :: i, j, pivot

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

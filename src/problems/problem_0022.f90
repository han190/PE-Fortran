submodule(module_problem) submodule_euler0022
implicit none
contains

module subroutine euler0022(problem)
  class(problem_type), intent(inout) :: problem
  character(len=20), parameter :: default = "N/A"
  integer(int64) :: i, n, unit, istat
  character(len=:), allocatable :: names(:)

  allocate (character(len=20) :: names(6000))
  names = default

  open (newunit=unit, file=problem%file, status="old", action="read")
  read (unit, *, iostat=istat) names
  close (unit)

  n = count(names /= default)
  call quicksort(names, 1_int64, n)
  write (problem%answer, "(i20)") sum([(i*score_letters(names(i)), i=1, n)])
end subroutine euler0022

elemental integer(int64) function score_letters(str)
  character(len=*), intent(in) :: str
  integer(int64) :: i

  score_letters = sum([(iachar(str(i:i)) - 64, i=1, len_trim(str))])
end function score_letters

recursive subroutine quicksort(string_arr, low, high)
  character(len=*), dimension(:), intent(inout) :: string_arr
  integer(int64), intent(in) :: low, high
  integer(int64) :: pivot_loc

  if (low < high) then
    call partition(string_arr, low, high, pivot_loc)
    call quicksort(string_arr, low, pivot_loc - 1)
    call quicksort(string_arr, pivot_loc + 1, high)
  end if
end subroutine quicksort

pure subroutine partition(string_arr, low, high, pivot_loc)
  character(len=*), intent(inout) :: string_arr(:)
  integer(int64), intent(in) :: low, high
  integer(int64), intent(out) :: pivot_loc
  character(:), allocatable :: pivot
  integer(int64) :: i, j

  pivot = string_arr(high)
  i = low - 1
  do j = low, high
    if (string_arr(j) < pivot) then
      i = i + 1
      call swap(string_arr(i), string_arr(j))
    end if
  end do
  call swap(string_arr(i + 1), string_arr(high))
  pivot_loc = i + 1
end subroutine partition

end submodule submodule_euler0022


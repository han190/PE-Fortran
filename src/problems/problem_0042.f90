submodule(module_problem) submodule_euler0042
implicit none
contains

module subroutine euler0042(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), parameter :: score_max = 26*50
  logical :: is_triangular(score_max)
  character(len=:), allocatable :: names(:)
  integer(int64) :: unit, iostat, i, sln

  open (newunit=unit, file=problem%file, action="read", status="old")
  allocate (character(len=50) :: names(2000))
  names = "n/a"
  read (unit, *, iostat=iostat) names
  close (unit)

  call triangular_number(score_max, is_triangular)
  sln = 0
  do i = 1, count(names /= "n/a")
    if (is_triangular(score(names(i)))) sln = sln + 1
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0042

pure integer(int64) function score(str)
  character(len=*), intent(in) :: str
  integer(int64) :: i

  score = sum([(iachar(str(i:i)) - 64, i=1, len_trim(str))])
end function score

pure subroutine triangular_number(n, is_triangular)
  integer(int64), intent(in) :: n
  logical, intent(out) :: is_triangular(n)
  integer(int64) :: i

  is_triangular = .false.
  i = 1
  do while (i*(i + 1)/2 <= n)
    is_triangular(i*(i + 1)/2) = .true.
    i = i + 1
  end do
end subroutine triangular_number

end submodule submodule_euler0042

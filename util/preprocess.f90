program main
implicit none

character(len=*), parameter :: src = "./src/", build = "./build/"
integer, allocatable :: problems(:), datasets(:)
integer :: i, unit

problems = read_numbers(build//"solved_problems.txt", "problem_")
datasets = read_numbers(build//"required_datasets.txt", "data_")

!> Generate interfaces
open (newunit=unit, file=src//"interface.inc", action="write")
write (unit, "(a)") "!> Automatically generated."
do i = 1, size(problems)
  write (unit, "(a, i4.4, a)") "module subroutine euler", problems(i), "(problem)"
  write (unit, "(2x, a)") "class(problem_type), intent(inout) :: problem"
  write (unit, "(a, i4.4)") "end subroutine euler", problems(i)
end do
close (unit)

!> Generate pointer associations
open (newunit=unit, file=src//"problem.inc", action="write")
write (unit, "(a)") "!> Automatically generated."
write (unit, "(a, i0, a)") "allocate (problems(", size(problems), "))"
do i = 1, size(problems)
  write (unit, "(a, i0, a, i4.4)") "problems(", i, ")%solve => euler", problems(i)
  write (unit, "(a, i0, a, i0)") "problems(", i, ")%index = ", problems(i)
  if (any(problems(i) == datasets)) write (unit, "(a, i0, a, i4.4, a)") &
    & "problems(", i, ")%file = data_dir//'/'//'data_", problems(i), ".txt'"
end do
close (unit)

contains

function read_numbers(filename, pattern) result(numbers)
  character(len=*), intent(in) :: filename, pattern
  integer, allocatable :: numbers(:)
  integer :: idx, fileunit, iostat, num_lines, loc
  character(len=100) :: tmp

  num_lines = 0
  open (newunit=fileunit, file=filename, &
    & action="read", iostat=iostat)
  do
    read (fileunit, *, iostat=iostat)
    if (iostat /= 0) exit
    num_lines = num_lines + 1
  end do

  allocate (numbers(num_lines))
  numbers = 0
  rewind (fileunit)
  do idx = 1, num_lines
    read (fileunit, "(a)") tmp
    loc = index(tmp, pattern)
    if (loc /= 0) then
      loc = loc + len(pattern)
      read (tmp(loc:loc + 3), "(i4.4)") numbers(idx)
    end if
  end do
  close (fileunit)
  numbers = pack(numbers, numbers /= 0)
end function read_numbers

end program

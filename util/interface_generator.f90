program main
implicit none

character(len=*), parameter :: src = "./src/", data = "./data/"
character(len=*), parameter :: build = "./build/"
integer, allocatable :: problems(:), datasets(:)
character(len=:), allocatable :: sel_problems, sel_datasets
integer :: i, unit

sel_problems = build//"selected_problems.txt"
sel_datasets = build//"selected_datasets.txt"
!> Assuming linux/mac
#if defined (_WIN32)
error stop "[Interface Generator] Currently does not support windows."
#else
call execute_command_line("mkdir -p "//build)
call execute_command_line("ls "//src//"problems/problem_????.f90"//" > "//sel_problems)
call execute_command_line("ls "//data//"data_????.txt"//" > "//sel_datasets)
#endif

problems = read_numbers(sel_problems, 23)
datasets = read_numbers(sel_datasets, 12)

!> Generate interfaces
open (newunit=unit, file=src//"interface.inc")
do i = 1, size(problems)
  write (unit, "(a, i4.4, a)") "module subroutine euler", problems(i), "(problem)"
  write (unit, "(2x, a)") "class(problem_type), intent(inout) :: problem"
  write (unit, "(a, i4.4)") "end subroutine euler", problems(i)
end do
close (unit)

!> Generate pointer associations
open (newunit=unit, file=src//"problem.inc")
write (unit, "(a, i0, a)") "allocate (problems(", size(problems), "))"
do i = 1, size(problems)
  write (unit, "(a, i0, a, i4.4)") "problems(", i, ")%solve => euler", problems(i)
  write (unit, "(a, i0, a, i0)") "problems(", i, ")%index = ", problems(i)
  if (any(problems(i) == datasets)) &
    & write (unit, "(a, i0, a, i4.4, a)") &
    & "problems(", i, ")%file = 'data_", problems(i), ".txt'"
end do
close (unit)

contains

function read_numbers(filename, num_headers) result(numbers)
  character(len=*), intent(in) :: filename
  integer, intent(in) :: num_headers
  integer, allocatable :: numbers(:)
  integer :: i, unit, num_lines, iostat
  character(len=num_headers) :: head
  character(len=3) :: ext

  open (newunit=unit, file=filename, iostat=iostat)
  num_lines = 0
  do
    read (unit, *, iostat=iostat)
    if (iostat /= 0) exit
    num_lines = num_lines + 1
  end do

  allocate (numbers(num_lines))
  rewind (unit)
  do i = 1, num_lines
    read (unit, "(a, i4.4, a3)") head, numbers(i), ext
  end do
  close (unit)
end function read_numbers

end program

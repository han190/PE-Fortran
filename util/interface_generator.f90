program main
implicit none

character(len=*), parameter :: src = "./src/"
character(len=*), parameter :: build = "./build/"
character(len=8) :: head
character(len=3) :: ext
integer, allocatable :: problems(:)
integer :: i, unit, num_lines, iostat
character(len=:), allocatable :: file

file = build//"selected_problems.txt"
!> Assuming linux/mac
#if defined (_WIN32)
error stop "[Interface Generator] Currently does not support windows."
#else
call execute_command_line("mkdir -p "//build)
call execute_command_line("ls "//src//"problems"//" > "//file)
#endif

!> Count number of lines
open (newunit=unit, file=file, iostat=iostat)
num_lines = 0
do
  read (unit, *, iostat=iostat)
  if (iostat /= 0) exit
  num_lines = num_lines + 1
end do

!> Figure out selected problems
allocate (problems(num_lines))
rewind (unit)
open (newunit=unit, file=file, iostat=iostat)
do i = 1, num_lines
  read (unit, "(a8, i4.4, a3)") head, problems(i), ext
end do
close (unit)

!> Generate interfaces
open (newunit=unit, file=src//"interface.inc")
do i = 1, size(problems)
  write (unit, "(a, i4.4, a)") "module subroutine euler", problems(i), "(problem)"
  write (unit, "(2x, a)") "type(problem_type), intent(inout) :: problem"
  write (unit, "(a, i4.4)") "end subroutine euler", problems(i)
end do
close (unit)

!> Generate pointer associations
open (newunit=unit, file=src//"solutions.inc")
write (unit, "(a, i0, a)") "allocate (solutions(", size(problems), "))"
do i = 1, size(problems)
  write (unit, "(a, i0, a, i4.4)") "solutions(", i, ")%solve => euler", problems(i)
  write (unit, "(a, i0, a, i0)") "solutions(", i, ")%index = ", problems(i)
end do
close (unit)

end program

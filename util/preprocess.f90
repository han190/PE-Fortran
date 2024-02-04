module module_preprocess
implicit none

public :: scan_folders
private

contains

subroutine list_files(dir, pattern, output)
  character(len=*), intent(in) :: dir, pattern, output
  character(len=100) :: command

  write (command, "('mkdir -p', 1x, a)") dir
  call execute_command_line(trim(command))
  write (command, "('ls', 1x, a, '/', a, 1x, '>', a)") &
    & dir, pattern//"*", output
  call execute_command_line(trim(command))
end subroutine list_files

function read_numbers(filepath, pattern) result(numbers)
  character(len=*), intent(in) :: filepath, pattern
  integer, allocatable :: numbers(:)
  integer :: i, unit, iostat, num_lines, loc
  character(len=100) :: tmp

  num_lines = 0
  open (newunit=unit, file=filepath, &
    & action="read", iostat=iostat)
  do
    read (unit, *, iostat=iostat)
    if (iostat /= 0) exit
    num_lines = num_lines + 1
  end do

  allocate (numbers(num_lines))
  numbers = 0
  rewind (unit)
  do i = 1, num_lines
    read (unit, "(a)") tmp
    loc = index(tmp, pattern)
    if (loc /= 0) then
      loc = loc + len(pattern)
      read (tmp(loc:loc + 3), "(i4.4)") numbers(i)
    end if
  end do
  close (unit)
  numbers = pack(numbers, numbers /= 0)
end function read_numbers

subroutine scan_folders(dir, pattern, res)
  character(len=*), intent(in) :: dir, pattern
  integer, allocatable, intent(out) :: res(:)
  character(len=*), parameter :: tmp = "ls.tmp"

  call list_files(dir, pattern, tmp)
  res = read_numbers(tmp, pattern)
  call execute_command_line("rm -rf "//tmp)
end subroutine scan_folders
end module module_preprocess

program main
  use, non_intrinsic :: module_preprocess
  implicit none

  character(len=*), parameter :: src = "src/"
  character(len=*), parameter :: warning = "Automatically generated file."
  character(len=*), parameter :: PP = "problemset%problems("
  integer, allocatable :: problems(:), datasets(:)
  integer :: i, unit

  !> Scan solved problems and dataset required.
  call scan_folders("src/problems", "problem_", problems)
  call scan_folders("data", "data_", datasets)

  !> Generate interfaces
  open (newunit=unit, file=src//"interface.inc", action="write")
  write (unit, "('!>', 1x, a)") warning
  write (unit, "('!>', 1x, a)") repeat('-', len(warning))
  do i = 1, size(problems)
    write (unit, "(a, i4.4, a)") "module subroutine euler", &
      & problems(i), "(problem)"
    write (unit, "(2x, a)") "type(problem_type), intent(inout) :: problem"
    write (unit, "(a, i4.4)") "end subroutine euler", problems(i)
  end do
  close (unit)

  !> Generate pointer associations
  open (newunit=unit, file=src//"problemset.inc", action="write")
  write (unit, "('!>', 1x, a)") warning
  write (unit, "('!>', 1x, a)") repeat('-', len(warning))
  write (unit, "(a, i0)") "problemset%num_problems = ", size(problems)
  write (unit, "(a)") "allocate (problemset%solutions(problemset%num_problems))"
  write (unit, "(a)") "allocate (problemset%problems(problemset%num_problems))"
  do i = 1, size(problems)
    write (unit, "(a, i0, a, i4.4)") "problemset%solutions(", &
      & i, ")%solve => euler", problems(i)
    write (unit, "(a, i0, a, i0)") PP, i, ")%index = ", problems(i)
    if (any(problems(i) == datasets)) then
      write (unit, "(a, i0, a, i4.4, a)") PP, i, &
        & ")%file = data_directory//'/'//'data_", problems(i), ".txt'"
    else
      write (unit, "(a, i0, a)") PP, i, ")%file = ''"
    end if
    write (unit, "(a, i0, a)") PP, i, ")%answer = ''"
  end do
  close (unit)
end program

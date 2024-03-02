program main
  use, non_intrinsic :: module_file
  implicit none

  character(len=*), parameter :: src = "src/"
  character(len=*), parameter :: warning = "Automatically generated file."
  character(len=*), parameter :: PP = "problemset%problems("
  character(len=*), parameter :: PS = "problemset%solutions("
  character(len=:), allocatable :: tmp
  integer, allocatable :: problems(:), datasets(:)
  integer :: i, unit

  !> Scan solved problems and dataset required.
  problems = find_indexed("src/problems", "problem")
  datasets = find_indexed("data", "data")

  !> Generate interfaces
  open (newunit=unit, file=src//"interface.inc", action="write")
  write (unit, "('!>', 1x, a)") warning
  write (unit, "('!>', 1x, a)") repeat('-', len(warning))
  do i = 1, size(problems)
    write (unit, "(a, i4.4, a)") "module subroutine euler", problems(i), "(problem)"
    write (unit, "(2x, a)") "type(problem_type), intent(inout) :: problem"
    write (unit, "(a, i4.4)") "end subroutine euler", problems(i)
  end do
  close (unit)

  !> Generate pointer associations
  open (newunit=unit, file=src//"problemset.inc", action="write")
  write (unit, "('!>', 1x, a)") warning
  write (unit, "('!>', 1x, a)") repeat('-', len(warning))
  write (unit, "(a, i0)") "problemset%num_problems = ", size(problems)
  write (unit, "(a)") "allocate ("//PS//"problemset%num_problems))"
  write (unit, "(a)") "allocate ("//PP//"problemset%num_problems))"

  tmp = ")%file = data_directory//'/'//'data_"
  do i = 1, size(problems)
    write (unit, "(a, i0, a, i4.4)") PS, i, ")%solve => euler", problems(i)
    write (unit, "(a, i0, a, i0)") PP, i, ")%index = ", problems(i)
    write (unit, "(a, i0, a)") PP, i, ")%answer = ''"
    if (any(problems(i) == datasets)) &
      & write (unit, "(a, i0, a, i4.4, a)") PP, i, tmp, problems(i), ".txt'"
  end do
  close (unit)
end program

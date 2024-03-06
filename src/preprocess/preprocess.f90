program main
  use, non_intrinsic :: module_file
  use, non_intrinsic :: module_preprocessor
  implicit none

  character(len=*), parameter :: src = "src/"
  character(len=:), allocatable :: tmp, tmp2, srcs(:), arguments(:)
  character(len=9) :: name
  integer, allocatable :: problems(:), datasets(:)
  integer :: i, j, unit, argument_counts, indent = 2
  type(declaration_type) :: decl
  type(subroutine_type) :: sub

  !> Read data directory if specified.
  argument_counts = command_argument_count()
  if (.not. (argument_counts == 2 .or. argument_counts == 0)) &
    & error stop "[PROJECT EULER] Invalid argument count."
  
  if (argument_counts == 2) then
    allocate (character(len=500) :: arguments(argument_counts))
    do i = 1, argument_counts
      call get_command_argument(i, arguments(i))
    end do

    select case (trim(arguments(1)))
    case ("-d", "--data")
      open (newunit=unit, file=src//"directory.inc", action="write")
      decl = declaration_type(name="default_data_directory", &
        & type="character(len=*)", attrs=["parameter"])
      write (unit, "(a, 2(1x, a))") declaration_to_src(decl), &
        & '=', '"'//trim2(arguments(2))//'"'
      close (unit)
    end select
  end if

  !> Scan solved problems and dataset required.
  problems = find_indexed("src/problems", "problem")
  datasets = find_indexed("data", "data")

  !> Generate interfaces
  open (newunit=unit, file=src//"interface.inc", action="write")
  write (unit, "(a)") "!AUTOMATICALLY GENERATTED!"
  decl = declaration_type(name="problem", &
    & type="type(problem_type)", attrs=["intent(inout)"])
  sub = subroutine_type(prefix="module", name=name, args=[decl])
  do i = 1, size(problems)
    write (sub%name, "('euler', i4.4)") problems(i)
    srcs = subroutine_to_src(sub, indent)
    write (unit, "(a)") (trim(srcs(j)), j=1, size(srcs))
  end do
  close (unit)

  !> Generate procedure pointers and initialize problem types
  allocate (character(len=500) :: tmp, tmp2)
  open (newunit=unit, file=src//"problemset.inc", action="write")
  write (unit, "(a)") "!AUTOMATICALLY GENERATTED!"
  write (unit, "(a, 1x, '=', 1x, i0)") "problemset%num_problems", size(problems)
  tmp = "('allocate (', a, '(', i0, '))')"
  write (unit, trim(tmp)) "problemset%problems", size(problems)
  write (unit, trim(tmp)) "problemset%solutions", size(problems)
  
  do i = 1, size(problems)
    write (name, "('euler', i4.4)") problems(i)
    write (tmp, "(a, '(', i0, ')%solve')") "problemset%solutions", i
    write (unit, "(a, 2(1x, a))") trim2(tmp), '=>', name
    write (tmp, "(a, '(', i0, ')%index')") "problemset%problems", i
    write (unit, "(a, 1x, '=', 1x, i0)") trim2(tmp), problems(i)
    if (any(problems(i) == datasets)) then
      write (name, "('data_', i4.4)") problems(i)
      write (tmp, "(a, '(', i0, ')%file')") "problemset%problems", i
      write (tmp2, "(a)") "data_directory//'/"//name//".txt'"
      write (unit, "(a, 2(1x, a))") trim2(tmp), '=', trim2(tmp2)
    end if
  end do
  close (unit)
end program

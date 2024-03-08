program main
  use, non_intrinsic :: module_file
  use, non_intrinsic :: module_preprocessor
  implicit none

  character(len=:), allocatable :: tmp, tmp2, srcs(:), arguments(:)
  character(len=:), allocatable :: data_dir, include_dir
  character(len=9) :: name
  integer, allocatable :: problems(:), datasets(:)
  integer :: i, j, unit, argument_counts, indent = 2
  type(declaration_type) :: decl1, decl2
  type(subroutine_type) :: sub

  !> Read data directory if specified.
  argument_counts = command_argument_count()
  if (.not. any(argument_counts == [0, 2, 4])) &
    & error stop "[PE-Preprocess] Invalid argument count."
  
  if (argument_counts >= 2) then
    allocate (character(len=500) :: arguments(argument_counts))
    do i = 1, argument_counts
      call get_command_argument(i, arguments(i))
    end do

    do i = 1, argument_counts, 2
      select case (trim2(arguments(i)))
      case ("-d", "--data")
        data_dir = trim2(arguments(i + 1))
      case ("-i", "--include")
        include_dir = trim2(arguments(i + 1))
      case default
        error stop "[PE-Preprocess] Invalid argument."
      end select
    end do
  end if

  if (.not. allocated(include_dir)) include_dir = "src"
  if (allocated(data_dir)) then
    open (newunit=unit, file=include_dir//"/"//"directory.inc", action="write")
    decl1 = declaration_type(name="default_data_directory", &
      & type="character(len=*)", attrs=["parameter"])
    write (unit, "(a, 2(1x, a))") declaration_to_src(decl1), &
      & '=', '"'//trim2(arguments(2))//'"'
    close (unit)
  end if

  !> Scan solved problems and dataset required.
  problems = find_indexed("src/problems", "problem")
  datasets = find_indexed("data", "data")

  !> Generate interfaces
  open (newunit=unit, file=include_dir//"/"//"interface.inc", action="write")
  write (unit, "(a)") "!AUTOMATICALLY GENERATTED!"
  decl1 = declaration_type(name="answer", &
    & type="character(len=*)", attrs=["intent(out)"])
  decl2 = declaration_type(name="file", &
    & type="character(len=*)", attrs=["intent(in)"])
  sub = subroutine_type(prefix="module", name=name, args=[decl1, decl2])
  do i = 1, size(problems)
    write (sub%name, "('euler', i4.4)") problems(i)
    srcs = subroutine_to_src(sub, indent)
    write (unit, "(a)") (trim(srcs(j)), j=1, size(srcs))
  end do
  close (unit)

  !> Generate procedure pointers and initialize problem types
  allocate (character(len=500) :: tmp, tmp2)
  open (newunit=unit, file=include_dir//"/"//"problemset.inc", action="write")
  write (unit, "(a)") "!AUTOMATICALLY GENERATTED!"
  tmp = "('allocate (', a, '(', i0, '))')"
  write (unit, trim(tmp)) "problems", size(problems)
  
  do i = 1, size(problems)
    write (name, "('euler', i4.4)") problems(i)
    write (tmp, "(a, '(', i0, ')%solve')") "problems", i
    write (unit, "(a, 2(1x, a))") trim2(tmp), '=>', name
    write (tmp, "(a, '(', i0, ')%index')") "problems", i
    write (unit, "(a, 1x, '=', 1x, i0)") trim2(tmp), problems(i)

    write (tmp, "(a, '(', i0, ')%file')") "problems", i
    write (name, "('data_', i4.4)") problems(i)
    if (any(problems(i) == datasets)) then
      tmp2 = "data_directory//'/"//name//".txt'"
    else
      tmp2 = "''"
    end if
    write (unit, "(a, 2(1x, a))") trim2(tmp), '=', trim2(tmp2)
  end do
  close (unit)
end program

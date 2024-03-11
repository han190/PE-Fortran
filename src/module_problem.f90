module module_problem

use, intrinsic :: iso_fortran_env, only: int64, real64, output_unit
use, non_intrinsic :: euler_toolkit
implicit none

public :: problem_type
public :: new_problems
public :: solve_problems
public :: print_answers
public :: list_problems
public :: default_data_directory
private

!> Problem type
type :: problem_type
  procedure(solve_proc), nopass, pointer :: solve => null()
  integer(int64) :: index = 0
  character(len=:), allocatable :: file
  real(real64) :: time_span = 0.0
  character(len=20) :: answer = ""
end type problem_type

!> Interface of a solution procedure
abstract interface
  subroutine solve_proc(answer, file)
    character(len=*), intent(out) :: answer
    character(len=*), intent(in) :: file
  end subroutine solve_proc
end interface

!> Interface to submodules
interface
  include "interface.inc"
end interface

!> Commonly used parameters
character, parameter :: carriage_return = char(13)
character, parameter :: space = char(32)
include "directory.inc"

contains

!> Construct problem set
function new_problems(data_directory) result(problems)
  character(len=*), intent(in) :: data_directory
  type(problem_type), allocatable :: problems(:)

  include "problemset.inc"
end function new_problems

!> Solve problem
subroutine solve_problem(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: count_rate, clock_start, clock_end

  if (.not. associated(problem%solve)) error stop &
    & "[solve_problem] solution not associated."

  call system_clock(count=clock_start, count_rate=count_rate)
  call problem%solve(problem%answer, problem%file)
  call system_clock(count=clock_end)
  problem%time_span = real(clock_end - clock_start, real64)/count_rate
  problem%answer = adjustl(problem%answer) !> Format answer
end subroutine solve_problem

!> Solve problems
subroutine solve_problems(problems, num_trails, selected)
  type(problem_type), intent(inout) :: problems(:)
  integer(int64), intent(in) :: num_trails, selected
  integer(int64) :: i, j, step, num_steps, num_problems
  real(real64) :: time_span, percent
  character(len=:), allocatable :: output_format

  output_format = "(a1, '[', i0, '%]', 1x, 'Solving P', i0)"
  num_problems = size(problems)
  select case (selected)
  case (0) !> Solve all available problems

    num_steps = num_problems*num_trails
    do i = 1, num_problems
      time_span = 0.0
      do j = 1, num_trails
        step = (i - 1)*num_trails + j
        percent = real(step)/real(num_steps)*100.0
        write (output_unit, output_format, advance="no") &
          & carriage_return, int(percent), problems(i)%index
        flush (output_unit)
        call solve_problem(problems(i))
        time_span = time_span + problems(i)%time_span
      end do
      problems(i)%time_span = time_span/num_trails
    end do

    output_format = "(a, '[', i0, '%]', 1x, i0, 1x, a)"
    write (output_unit, output_format, advance="no") &
      & carriage_return, 100, num_problems, &
      & "problems solved."//new_line("(a)")

  case (1:) !> Solve a single problems

    do i = 1, num_problems
      if (problems(i)%index == selected) exit
    end do
    if (i == num_problems + 1) error stop &
      & "[solve_problem] Problem not found."
    time_span = 0.0

    do j = 1, num_trails
      percent = real(j)/real(num_trails)*100.0
      write (output_unit, output_format, advance="no") &
        & carriage_return, int(percent), problems(i)%index
      flush (output_unit)
      call solve_problem(problems(i))
      time_span = time_span + problems(i)%time_span
    end do

    problems(i)%time_span = time_span/num_trails
    write (output_unit, "(a1)") carriage_return
    flush (output_unit)
    write (output_unit, "('Problem:', 1x, i0)") problems(i)%index
    write (output_unit, "('Solution:', 1x, a)") adjustl(problems(i)%answer)
    output_format = "('Time span:', 1x, es0.4e2, 1x, '(sec)')"
    write (output_unit, output_format) problems(i)%time_span

  case default
    error stop "[solve_problems] Invalid selected."
  end select
end subroutine solve_problems

!> Relative difficulty
elemental function relative_difficulty(time_span, time_min, time_max)
  real(real64), intent(in) :: time_span, time_min, time_max
  character(len=20) :: relative_difficulty
  real(real64) :: score

  score = log10((time_min*10 - time_max)/(time_min - time_max) + &
    & (1 - 10)*time_span/(time_min - time_max))/log10(10.0)
  associate (num_bars => nint(score*10))
    write (relative_difficulty, "(a, 1x, '(', i0, '%', ')')") &
      & repeat('|', num_bars + 1), nint(score*100)
  end associate
end function relative_difficulty

!> List all solved problems
subroutine list_problems(problems)
  type(problem_type), intent(in) :: problems(:)
  integer(int64) :: i, j, s
  integer(int64), allocatable :: indices(:)
  integer(int64) :: num_problems
  logical, allocatable :: solved(:)
  character(len=:), allocatable :: output_format

  num_problems = 0
  do while (num_problems < size(problems))
    num_problems = num_problems + 50
  end do

  allocate (solved(num_problems))
  solved = .false.
  indices = [(problems(i)%index, i=1, size(problems))]
  solved(indices) = .true.

  s = 5
  output_format = "(*(i4, 1x, a1))"
  do i = 1, num_problems, s
    write (output_unit, output_format) &
      & (j, merge("x", " ", solved(j)), j=i, i + s - 1)
  end do
  output_format = "('[', i0, 1x, 'problems solved]')"
  write (output_unit, output_format) size(problems)
end subroutine list_problems

!> Write answers to file
subroutine print_answers(problems, file)
  type(problem_type), intent(in) :: problems(:)
  character(len=*), intent(in) :: file
  integer(int64) :: unit, i, num_problems
  character(len=:), allocatable :: difficulties(:)
  real(real64) :: time_min, time_max, time_tot
  character(len=:), allocatable :: message, output_format

  time_min = huge(0.0_real64)
  time_max = tiny(0.0_real64)
  time_tot = 0.0
  num_problems = 0
  do i = 1, size(problems)
    time_min = min(problems(i)%time_span, time_min)
    time_max = max(problems(i)%time_span, time_max)
    time_tot = time_tot + problems(i)%time_span
    num_problems = num_problems + 1
  end do

  difficulties = [(relative_difficulty(problems(i)%time_span, &
    & time_min, time_max), i=1, size(problems))]
  open (newunit=unit, file=file, action='write', status='unknown')
  output_format = "('#', t6, a, t35, a, t63, a)"
  write (unit, output_format) 'Answer', 'Time (sec)', 'Difficulty'
  write (unit, "(a)") repeat('-', 80)
  output_format = "(i0, t6, a, t35, es0.4e2, 1x, t63, a)"
  write (unit, output_format) (problems(i)%index, problems(i)%answer, &
    & problems(i)%time_span, trim(difficulties(i)), i=1, size(problems))
  write (unit, "(a)") repeat('-', 80)
  message = "Number of problems solved"
  write (unit, "('*', t6, a, t35, i0)") message, num_problems
  message = "Mean time (sec) / problem"
  output_format = "('*', t6, a, t35, es0.4e2)"
  write (unit, output_format) message, time_tot/num_problems
  close (unit)
  write (output_unit, "(a)") "File "//file//" generated."
end subroutine print_answers

end module module_problem

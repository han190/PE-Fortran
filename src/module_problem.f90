module module_problem

use, intrinsic :: iso_fortran_env, only: int64, real64, output_unit
use :: euler_toolkit
implicit none

public :: problemset_type
public :: new_problemset
public :: solve_problems
public :: print_answers
public :: list_problems
private

!> Problem type
type :: problem_type
  integer(int64) :: index = 0
  character(len=:), allocatable :: file
  real(real64) :: time_span = 0.0
  character(len=20) :: answer = ""
end type problem_type

!> Solution type
type :: solution_type
  procedure(solve_procedure), nopass, pointer :: solve => null()
end type solution_type

!> Problem set type
type :: problemset_type
  integer :: num_problems
  type(solution_type), allocatable :: solutions(:)
  type(problem_type), allocatable :: problems(:)
end type problemset_type

!> Interface of a solution procedure
abstract interface
  subroutine solve_procedure(problem)
    import :: problem_type
    type(problem_type), intent(inout) :: problem
  end subroutine solve_procedure
end interface

!> Interface to submodules
interface
  include "interface.inc"
end interface

!> Commonly used parameters
character(len=*), parameter :: carriage_return = char(13)
character(len=*), parameter :: space = char(32)

contains

!> Construct problem set
function new_problemset(data_directory) result(problemset)
  character(len=*), intent(in) :: data_directory
  type(problemset_type) :: problemset

  include "problemset.inc"
end function new_problemset

!> Solve problem
subroutine solve_problem(problem, solution)
  type(problem_type), intent(inout) :: problem
  type(solution_type), intent(in) :: solution
  integer(int64) :: count_rate, clock_start, clock_end

  call system_clock(count=clock_start, count_rate=count_rate)
  call solution%solve(problem)
  call system_clock(count=clock_end)
  problem%time_span = real(clock_end - clock_start, real64)/count_rate
  problem%answer = adjustl(problem%answer) !> Format answer
end subroutine solve_problem

!> Solve problems
subroutine solve_problems(problemset, num_trails, selected)
  type(problemset_type), target, intent(in) :: problemset
  integer(int64), intent(in) :: num_trails, selected
  integer(int64) :: i, j, step, num_steps, num_problems
  real(real64) :: time_span, percent
  character(len=:), allocatable :: output_format
  type(solution_type), pointer :: solution => null()
  type(problem_type), pointer :: problem => null()

  output_format = "(a1, '[', i0, '%]', 1x, 'Solving P', i0)"
  num_problems = problemset%num_problems
  if (selected /= 0) then
    do i = 1, num_problems
      if (problemset%problems(i)%index == selected) exit
    end do
    if (i == num_problems + 1) error stop &
      & "[solve_problem] Problem not found."
    solution => problemset%solutions(i)
    problem => problemset%problems(i)
    time_span = 0.0
    do j = 1, num_trails
      percent = real(j)/num_trails*100.0
      write (output_unit, output_format, advance="no") &
        & carriage_return, int(percent), problem%index
      flush (output_unit)
      call solve_problem(problem, solution)
      time_span = time_span + problem%time_span
    end do
    problem%time_span = time_span/num_trails
    write (output_unit, "(a1)") carriage_return
    flush (output_unit)
    output_format = "('Problem:', 1x, i0)"
    write (output_unit, output_format) problem%index
    output_format = "('Solution:', 1x, a)"
    write (output_unit, output_format) adjustl(problem%answer)
    output_format = "('Time span:', 1x, es0.4e3, 1x, '(sec)')"
    write (output_unit, output_format) problem%time_span
  else
    num_steps = num_problems*num_trails
    do i = 1, num_problems
      solution => problemset%solutions(i)
      problem => problemset%problems(i)
      time_span = 0.0
      do j = 1, num_trails
        step = (i - 1)*num_trails + j
        percent = real(step)/num_steps*100.0
        write (output_unit, output_format, advance="no") &
          & carriage_return, int(percent), problem%index
        flush (output_unit)
        call solve_problem(problem, solution)
        time_span = time_span + problem%time_span
      end do
      problem%time_span = time_span/num_trails
    end do
    write (output_unit, "(a1)") carriage_return
    flush (output_unit)
    output_format = "(i0, 1x, 'problems solved.')"
    write (output_unit, output_format) num_problems
  end if
  nullify (problem, solution)
end subroutine solve_problems

!> Relative difficulty
elemental function relative_difficulty( &
  & time_span, time_min, time_max) result(ret)
  real(real64), intent(in) :: time_span, time_min, time_max
  character(len=10) :: ret
  integer(int64) :: level, level_max
  real(real64) :: normalized

  level = 0
  level_max = 8
  normalized = (time_span - time_min)/(time_max - time_min)
  do while (normalized < 10.0**(-level) .and. level < level_max)
    level = level + 1
  end do
  level = level_max - level
  write (ret, "('[', a8, ']')") repeat('|', level)// &
    & repeat(space, level_max - level)
end function relative_difficulty

!> List all solved problems
subroutine list_problems(problemset)
  type(problemset_type), intent(in) :: problemset
  integer(int64) :: i, j, s
  integer(int64), allocatable :: indices(:)
  integer(int64) :: num_problems
  logical, allocatable :: solved(:)
  character(len=:), allocatable :: output_format

  num_problems = 0
  do while (num_problems < problemset%num_problems)
    num_problems = num_problems + 50
  end do

  allocate (solved(num_problems))
  solved = .false.
  associate (problems => problemset%problems)
    indices = [(problems(i)%index, &
      & i=1, problemset%num_problems)]
  end associate
  solved(indices) = .true.

  s = 5
  output_format = "(*(i4, 1x, a1))"
  do i = 1, num_problems, s
    write (output_unit, output_format) &
      & (j, merge("x", " ", solved(j)), j=i, i + s - 1)
  end do
  output_format = "('[', i0, 1x, 'problems solved]')"
  write (output_unit, output_format) problemset%num_problems
end subroutine list_problems

!> Write answers to file
subroutine print_answers(problemset, file)
  type(problemset_type), target, intent(in) :: problemset
  character(len=*), intent(in) :: file
  integer(int64) :: unit, i, difficulty, num_problems
  character(len=10), allocatable :: difficulties(:)
  real(real64) :: time_min, time_max, time_tot
  character(len=:), allocatable :: message, output_format
  type(problem_type), pointer :: problems(:) => null()

  time_min = huge(0.0_real64)
  time_max = tiny(0.0_real64)
  time_tot = 0.0
  num_problems = 0
  problems => problemset%problems
  do i = 1, size(problems)
    time_min = min(problems(i)%time_span, time_min)
    time_max = max(problems(i)%time_span, time_max)
    time_tot = time_tot + problems(i)%time_span
    num_problems = num_problems + 1
  end do

  difficulties = [(relative_difficulty(problems(i)%time_span, &
    & time_min, time_max), i=1, size(problems))]
  open (newunit=unit, file=file, action='write', status='unknown')
  output_format = "('#', t6, a, t40, a, t63, a)"
  write (unit, output_format) 'Answer', 'Time (sec)', 'Difficulty'
  write (unit, "(a)") repeat('-', 72)
  output_format = "(i0, t6, a, t40, es0.4e3, 1x, t63, a)"
  write (unit, output_format) (problems(i)%index, problems(i)%answer, &
    & problems(i)%time_span, difficulties(i), i=1, size(problems))
  write (unit, "(a)") repeat('-', 72)
  message = "Number of problems solved"
  write (unit, "('*', t6, a, t40, i0)") message, num_problems
  message = "Mean time (sec) / problem"
  output_format = "('*', t6, a, t40, es0.4e3)"
  write (unit, output_format) message, time_tot/num_problems
  close (unit)
  nullify (problems)
end subroutine print_answers

end module module_problem

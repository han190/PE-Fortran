module module_problem

use :: iso_fortran_env, only: int64, real64, output_unit
use :: euler_toolkit
implicit none

public :: problem_type
public :: new_problems
public :: solve_problem
public :: solve_problems
public :: print_answers
public :: list_problems
private

!> Problem type
type :: problem_type
  procedure(solve_procedure), pointer :: solve => null()
  integer(int64) :: index = 0
  character(len=:), allocatable :: file
  real(real64) :: time_span = 0.0
  character(len=20) :: answer = ""
end type problem_type

!> Interface of a solution procedure
abstract interface
  subroutine solve_procedure(problem)
    import :: problem_type
    class(problem_type), intent(inout) :: problem
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

!> Construct problem arrays
function new_problems(data_dir) result(problems)
  character(len=*), intent(in) :: data_dir
  type(problem_type), allocatable :: problems(:)

  include "problem.inc"
end function new_problems

!> Solve problem
subroutine solve_problem(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: count_rate, clock_start, clock_end

  call system_clock(count_rate=count_rate)
  call system_clock(count=clock_start)
  call problem%solve()
  call system_clock(count=clock_end)
  problem%time_span = real(clock_end - clock_start, real64)/count_rate
  problem%answer = adjustl(problem%answer) !> Format answer
end subroutine solve_problem

!> Solve problems
subroutine solve_problems(problems, num_trails, selected)
  type(problem_type), allocatable, target, intent(inout) :: problems(:)
  integer(int64), intent(in) :: num_trails, selected
  integer(int64) :: i, j, step, num_steps, num_problems
  real(real64) :: Tspan, percent
  character(len=:), allocatable :: output_format
  type(problem_type), pointer :: P => null()

  output_format = "(a1, '[', i0, '%]', 1x, 'Solving P', i0)"
  num_problems = size(problems)
  if (selected /= 0) then
    do i = 1, num_problems
      if (problems(i)%index == selected) exit
    end do
    if (i == num_problems + 1) error stop &
      & "[solve_problem] Problem not found."
    P => problems(i)
    Tspan = 0.0
    do j = 1, num_trails
      percent = real(j)/num_trails*100.0
      write (output_unit, output_format, advance="no") &
        & carriage_return, int(percent), P%index
      flush (output_unit)
      call solve_problem(P)
      Tspan = Tspan + P%time_span
    end do
    P%time_span = Tspan/num_trails
    write (output_unit, "(a1)") carriage_return
    flush (output_unit)
    output_format = "('Problem:', 1x, i0)"
    write (output_unit, output_format) P%index
    output_format = "('Solution:', 1x, a)"
    write (output_unit, output_format) adjustl(P%answer)
    output_format = "('Time span:', 1x, es0.4e3, 1x, '(sec)')"
    write (output_unit, output_format) P%time_span
  else
    num_steps = num_problems*num_trails
    do i = 1, num_problems
      P => problems(i)
      Tspan = 0.0
      do j = 1, num_trails
        step = (i - 1)*num_trails + j
        percent = real(step)/num_steps*100.0
        write (output_unit, output_format, advance="no") &
          & carriage_return, int(percent), P%index
        flush (output_unit)
        call solve_problem(P)
        Tspan = Tspan + P%time_span
      end do
      P%time_span = Tspan/num_trails
    end do
    write (output_unit, "(a1)") carriage_return
    flush (output_unit)
    output_format = "(i0, 1x, 'problems solved.')"
    write (output_unit, output_format) num_problems
  end if
  nullify (P)
end subroutine solve_problems

!> Relative difficulty
elemental function relative_difficulty( &
  & time_span, time_min, time_max) result(ret)
  real(real64), intent(in) :: time_span, time_min, time_max
  integer(int64) :: ret
  real(real64) :: normalized

  ret = 0
  normalized = (time_span - time_min)/(time_max - time_min)
  do while (normalized < 10.0**(-ret))
    ret = ret + 1
  end do
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
  integer(int64) :: unit, i, difficulty, num_problems
  real(real64) :: time_min, time_max, time_tot
  character(len=:), allocatable :: message, output_format

  time_min = huge(0.0_real64)
  time_max = tiny(0.0_real64)
  time_tot = 0.0
  num_problems = 0
  do i = 1, size(problems)
    if (.not. trim(problems(i)%answer) == "") then
      time_min = min(problems(i)%time_span, time_min)
      time_max = max(problems(i)%time_span, time_max)
      time_tot = time_tot + problems(i)%time_span
      num_problems = num_problems + 1
    end if
  end do

  output_format = "(i0, t6, a, t40, es0.4e3, 1x, a)"
  open (newunit=unit, file=file, action='write', status='unknown')
  write (unit, "('#', t6, 'Answer', t40, 'Time (sec)')")
  write (unit, "(a)") repeat('-', 50)
  do i = 1, size(problems)
    associate (P => problems(i))
      difficulty = relative_difficulty(P%time_span, time_min, time_max)
      write (unit, output_format) P%index, P%answer, P%time_span, &
        & merge("slow", repeat(space, len("slow")), difficulty < 2)
    end associate
  end do
  write (unit, "(a)") repeat('-', 50)
  message = "Number of problems solved"
  write (unit, "('*', t6, a, t40, i0)") message, num_problems
  message = "Mean time (sec) / problem"
  output_format = "('*', t6, a, t40, es0.4e3)"
  write (unit, output_format) message, time_tot/num_problems
  close (unit)
end subroutine print_answers

end module module_problem
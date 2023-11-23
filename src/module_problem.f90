module module_problem

use :: iso_fortran_env, only: int64, real64, output_unit
use :: euler_toolkit
implicit none

public :: problem_type
public :: new_problems
public :: solve_problem
public :: solve_problems
public :: print_answers
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
end subroutine solve_problem

!> Solve problems
subroutine solve_problems(problems, num_trails, selected)
  type(problem_type), allocatable, target, intent(inout) :: problems(:)
  integer(int64), intent(in) :: num_trails, selected
  integer(int64) :: i, j, step, num_steps, num_problems
  real(real64) :: Tspan, percent
  character(len=:), allocatable :: fmt
  type(problem_type), pointer :: P => null()

  fmt = "(a1, '[', i0, '%]', 1x, 'Solving P', i0)"
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
      write (output_unit, fmt, advance="no") &
        & carriage_return, int(percent), P%index
      flush (output_unit)
      call solve_problem(P)
      Tspan = Tspan + P%time_span
    end do
    P%time_span = Tspan/num_trails
    write (output_unit, "(a1)") carriage_return
    flush (output_unit)
    fmt = "('Problem:', 1x, i0)"
    write (output_unit, fmt) P%index
    fmt = "('Solution:', 1x, a)"
    write (output_unit, fmt) adjustl(P%answer)
    fmt = "('Time span:', 1x, es0.4e3, 1x, '(sec)')"
    write (output_unit, fmt) P%time_span
  else
    num_steps = num_problems*num_trails
    do i = 1, num_problems
      P => problems(i)
      Tspan = 0.0
      do j = 1, num_trails
        step = (i - 1)*num_trails + j
        percent = real(step)/num_steps*100.0
        write (output_unit, fmt, advance="no") &
          & carriage_return, int(percent), P%index
        flush (output_unit)
        call solve_problem(P)
        Tspan = Tspan + P%time_span
      end do
      P%time_span = Tspan/num_trails
    end do
    write (output_unit, "(a1)") carriage_return
    flush (output_unit)
    fmt = "(i0, 1x, 'problems solved.')"
    write (output_unit, fmt) num_problems
  end if
  nullify (P)
end subroutine solve_problems

!> Relative difficulty
elemental function relative_difficulty(time_span, time_min, time_max) result(ret)
  real(real64), intent(in) :: time_span, time_min, time_max
  integer(int64) :: ret, i
  real(real64) :: norm

  norm = (time_span - time_min)/(time_max - time_min)
  do i = 1, 5
    if (norm >= 10.**(-i)) exit
  end do
  ret = i
end function relative_difficulty

!> Write answers to file
subroutine print_answers(problems, file)
  type(problem_type), intent(in) :: problems(:)
  character(len=*), intent(in) :: file
  integer(int64) :: unit, i, difficulty, num_problems
  character(:), allocatable :: format_
  real(real64) :: time_min, time_max, time_tot
  character(len=*), parameter :: TC = "TC"
  character(len=:), allocatable :: label, message

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

  format_ = "(i0, t6, a, t40, es0.4e3, 1x, a)"
  open (newunit=unit, file=file, action='write', status='unknown')
  write (unit, "('#', t6, 'Answer', t40, 'Timespan (sec)')")
  write (unit, "(a)") repeat('-', 54)
  do i = 1, size(problems)
    associate (P => problems(i))
      difficulty = relative_difficulty(P%time_span, time_min, time_max)
      label = merge(TC, repeat(space, len(TC)), difficulty < 2)
      write (unit, format_) P%index, adjustl(P%answer), &
        & P%time_span, trim(label)
    end associate
  end do
  write (unit, "(a)") repeat('-', 54)
  message = "Number of problems solved"
  write (unit, "('*', t6, a, t40, i0)") message, num_problems
  message = "Mean time spent per problem (sec)"
  write (unit, "('*', t6, a, t40, es0.4e3)") message, time_tot/num_problems
  write (unit, "('*', t6, a)") "TC: time consuming"
  close (unit)
end subroutine print_answers

end module module_problem
module module_problem

use :: iso_fortran_env, only: int64, real64
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

contains

!> Construct problem arrays
function new_problems(data_dir) result(problems)
  character(len=*), intent(in) :: data_dir
  type(problem_type), allocatable :: problems(:)

  include "problem.inc"
end function new_problems

!> Solve problem
subroutine solve_problem(problem, num_trails)
  type(problem_type), intent(inout) :: problem
  integer(int64), intent(in) :: num_trails
  integer(int64) :: count_rate, clock_start, clock_end, i
  real(real64) :: time_span
  character(len=:), allocatable :: format_

  format_ = "('Solving problem', 1x, i4.4, 1x, 'trial', 1x, i4.4, '...', a1)"
  call system_clock(count_rate=count_rate)
  do i = 1, num_trails
    write (*, format_, advance='no') problem%index, i, char(13)
    call system_clock(count=clock_start)
    call problem%solve()
    call system_clock(count=clock_end)
    time_span = real(clock_end - clock_start, real64)/count_rate
    problem%time_span = problem%time_span + time_span
  end do
  problem%time_span = problem%time_span/num_trails
end subroutine solve_problem

!> Solve problems
subroutine solve_problems(problems, num_trails, selected)
  type(problem_type), allocatable, intent(inout) :: problems(:)
  integer(int64), intent(in) :: num_trails, selected
  character(len=:), allocatable :: clear
  integer(int64) :: i

  clear = repeat(char(32), 34)//char(13)
  if (selected /= 0) then
    do i = 1, size(problems)
      if (problems(i)%index == selected) exit
    end do
    if (i == size(problems) + 1) error stop &
      & "[solve_problem] Problem not found."
    associate (P => problems(i))
      call solve_problem(P, num_trails)
      write (*, "(a)", advance='no') clear
      write (*, "('Problem', t12, i0)") P%index
      write (*, "('Solution', t12, a)") adjustl(P%answer)
      write (*, "('Time span', t12, es0.4e3)") P%time_span
    end associate
  else
    do i = 1, size(problems)
      call solve_problem(problems(i), num_trails)
    end do
    write (*, "(a)", advance='no') clear
    write (*, "(i0, 1x, 'problems solved.')") size(problems)
  end if
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
  character(len=*), parameter :: TC = "TC", space = char(32)
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
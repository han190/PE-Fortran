module module_problem

use :: iso_fortran_env, only: int64, real64, &
  & compiler_version, compiler_options
implicit none

public :: problem_type
public :: solution_type
public :: new_problem
public :: new_problems
public :: solve_problem
public :: solve_problems
public :: answer_sheet
private

!> Problem type
type :: problem_type
  integer(int64) :: index = 0
  character(len=:), allocatable :: name
  character(len=:), allocatable :: file
  real(real64) :: time_span = 0.0
  character(len=20) :: answer = ""
end type problem_type

!> An array of procedure pointers
type :: solution_type
  integer(int64) :: index = 0
  procedure(solve_procedure), nopass, pointer :: solve => null()
end type solution_type

!> Interface of a solution procedure
abstract interface
  subroutine solve_procedure(problem)
    import :: problem_type
    type(problem_type), intent(inout) :: problem
  end subroutine solve_procedure
end interface

contains

!> Construct problem type with external file
pure function new_problem(index, name, file) result(problem)
  integer(int64), intent(in) :: index
  character(len=*), intent(in) :: name
  character(len=*), intent(in) :: file
  type(problem_type) :: problem

  problem%index = index
  problem%name = name
  problem%file = file
end function new_problem

!> Construct problem arrays
subroutine new_problems(problems, data_dir)
  type(problem_type), allocatable, intent(inout) :: problems(:)
  character(len=*), intent(in) :: data_dir
  integer(int64) :: unit, iostat, num_lines, i, idx1, idx2, problem_index
  character(len=100) :: problem_title, data_file, tmp
  
  open (newunit=unit, file=data_dir//"/"//"problems.txt", action='read', iostat=iostat)
  if (iostat /= 0) error stop "[new_problems] problems.txt not found."
  num_lines = 0
  do
    read (unit, *, iostat=iostat)
    if (iostat /= 0) exit
    num_lines = num_lines + 1
  end do

  if (allocated(problems)) deallocate (problems)
  allocate (problems(num_lines))
  rewind (unit)
  do i = 1, num_lines
    read (unit, "(a)") tmp
    idx1 = index(tmp, ",")
    idx2 = index(tmp, ",", back=.true.)
    read (tmp(1:idx1 - 1), *) problem_index
    problem_title = tmp(idx1 + 1:idx2 - 1)
    data_file = tmp(idx2 + 1:)
    problems(i) = new_problem(problem_index, &
      & trim(problem_title), &
      & data_dir//"/"//trim(data_file))
  end do
end subroutine new_problems

!> Solve problem
subroutine solve_problem(problem, solve, num_trails)
  type(problem_type), intent(inout) :: problem
  procedure(solve_procedure) :: solve
  integer(int64), intent(in) :: num_trails
  integer(int64) :: count_rate, clock_start, clock_end
  integer(int64) :: i
  real(real64) :: time_span
  character(len=:), allocatable :: format_

  format_ = "('Solving problem', 1x, i4.4, 1x, 'trial', 1x, i4.4, '...', a1)"
  call system_clock(count_rate=count_rate)
  do i = 1, num_trails
    write (*, format_, advance='no') problem%index, i, char(13)
    call system_clock(count=clock_start)
    call solve(problem)
    call system_clock(count=clock_end)
    time_span = real(clock_end - clock_start, real64)/count_rate
    problem%time_span = problem%time_span + time_span
  end do
  problem%time_span = problem%time_span/num_trails
end subroutine solve_problem

!> Solve problems
subroutine solve_problems(problems, solutions, num_trails)
  type(problem_type), intent(inout) :: problems(:)
  type(solution_type), intent(in) :: solutions(:)
  integer(int64), intent(in) :: num_trails
  integer(int64) :: i

  do i = 1, size(solutions)
    if (solutions(i)%index <= size(problems) .and. &
      & associated(solutions(i)%solve)) then
      call solve_problem(problems(solutions(i)%index), &
        & solutions(i)%solve, num_trails)
    end if
  end do
  write (*, "(a)", advance='no') repeat(char(32), 34)//char(13)
  write (*, "(i0, 1x, 'problems solved.')") size(solutions)
end subroutine solve_problems

!> Relative difficulty
elemental function relative_difficulty(time_span, time_min, time_max) result(ret)
  real(real64), intent(in) :: time_span, time_min, time_max
  integer(int64) :: ret, i
  real(real64) :: norm

  norm = (time_span - time_min)/(time_max - time_min)
  do i = 1, 5
    if (norm >= 10.**(-i)) then
      ret = i
      return
    end if
  end do
  ret = 6
end function relative_difficulty

!> Write answers to file
subroutine answer_sheet(problems, file)
  type(problem_type), intent(in) :: problems(:)
  character(len=*), intent(in) :: file
  integer(int64) :: unit, i, difficulty
  character(:), allocatable :: format_
  real(real64) :: time_min, time_max, time_tot
  character(len=*), parameter :: TC = "TC", space = char(32)
  character(len=:), allocatable :: label

  time_min = huge(0.0_real64)
  time_max = tiny(0.0_real64)
  time_tot = 0.0_real64
  do i = 1, size(problems)
    if (.not. trim(problems(i)%answer) == "") then
      time_min = min(problems(i)%time_span, time_min)
      time_max = max(problems(i)%time_span, time_max)
      time_tot = time_tot + problems(i)%time_span
    end if
  end do

  format_ = "(i0, t5, a, t50, a, t70, es20.4e3, 1x, a)"
  open (newunit=unit, file=file, action='write')
  write (unit, "('*', t5, 'Compiler version:', 1x, a)") compiler_version()
  write (unit, "('*', t5, a)") "TC: time consuming"//new_line("(a)")
  write (unit, "('#', t5, 'Problem', t50, 'Answer', t79, 'Timespan (sec)')")
  write (unit, "(a)") repeat('-', 92)
  do i = 1, size(problems)
    difficulty = relative_difficulty(problems(i)%time_span, time_min, time_max)
    label = merge(TC, repeat(space, len(TC)), difficulty < 2)
    write (unit, format_) problems(i)%index, trim(problems(i)%name), &
      & adjustl(trim(problems(i)%answer)), problems(i)%time_span, trim(label)
  end do
  write (unit, "(a)") repeat('-', 92)
  write (unit, "(t5, a, t70, i20)") "Number of problesm solved", size(problems)
  write (unit, "(t5, a, t70, es20.4e3)") "Mean time spent per problem (sec)", time_tot/size(problems)
  close (unit)
end subroutine answer_sheet

end module module_problem
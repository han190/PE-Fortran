module module_driver

use, intrinsic :: iso_fortran_env, only: int64, output_unit
use, non_intrinsic :: module_problem
implicit none

public :: get_arguments
private

contains

!> Print an allocatable character array.
subroutine print_characters(array)
  character(len=*), intent(in) :: array(:)
  integer(int64) :: i

  do i = 1, size(array)
    write (output_unit, "(a)") trim(array(i))
  end do
end subroutine print_characters

!> Get version
pure subroutine get_version(messages)
  character(len=:), allocatable, intent(inout) :: messages(:)

  messages = [character(len=80) :: &
    & 'Project Euler with Fortran', &
    & 'Version: 0.4.0', 'License: MIT', &
    & 'Copyright: Copyright 2019 - 2023, Han Tang', &
    & 'Homepage: https://github.com/han190/PE-Fortran', &
    & '']
end subroutine get_version

!> Print help
pure subroutine get_help(messages)
  character(len=:), allocatable, intent(inout) :: messages(:)

  messages = [character(len=80) :: &
    & 'Arguments:', &
    & '   p[roblem]<N>     Solve a single problem.', &
    & '   -v, --version    Print version.', &
    & '   -h, --help       Pop up this message.', &
    & '   -t, --trail      Number of trails.', &
    & '   -d, --data       Data directory.', &
    & '   -a, --answer     Answer sheet (output file).', &
    & '   -l, --list       List solved problems.', &
    & '', &
    & 'Examples:', &
    & '   1. Solve all problems: PE-Fortran', &
    & '   2. Solve all problems 10 times: PE-Fortran -t 10', &
    & '   3. Solve problem 10: PE-Fortran p10', &
    & '   4. Solve problem 10 100 times: PE-Fortran p10 -t 100', &
    & '   5. List solved problems: PE-Fortran -l', &
    & '']
end subroutine get_help

!> If a string contains only digit
pure function is_digit(string) result(ret)
  character(len=*), intent(In) :: string
  logical :: ret
  integer :: i, a

  ret = .true.
  do i = 1, len(string)
    a = ichar(string(i:i))
    if (.not. (a >= 48 .and. a <= 57)) then
      ret = .false.
      return
    end if
  end do
end function is_digit

!> Argument check for single problem solution.
function problem_found(argument, keywords, value) result(found)
  character(len=*), intent(in) :: argument, keywords(:)
  integer(int64), intent(inout) :: value
  logical :: found
  character(len=:), allocatable :: keyword, value_str
  integer(int64) :: i

  found = .false.
  do i = 1, size(keywords)
    keyword = trim(keywords(i))
    if (index(argument, keyword) == 1) then
      value_str = argument(len(keyword) + 1:)
      if (.not. is_digit(value_str)) &
        & error stop "[PROJECT EULER] Invalid format."
      read (value_str, *) value
      found = .true.
      return
    end if
  end do
end function problem_found

!> Get arugment
subroutine get_arguments()
  character(len=:), allocatable :: answer_sheet, data_directory, output_format
  character(len=:), allocatable :: arguments(:), keywords(:)
  character(len=:), allocatable :: argument, next_argument, messages(:)
  integer(int64) :: num_trails, selected
  integer :: argument_counts, i
  type(problem_type), allocatable :: problems(:)
  logical :: list_solved

  argument_counts = command_argument_count()
  if (argument_counts >= 10) &
    & error stop "[PROJECT EULER] Invalid argument count."
  allocate (character(len=500) :: arguments(argument_counts))
  do i = 1, argument_counts
    call get_command_argument(i, arguments(i))
  end do

  !> Default values
  data_directory = default_data_directory
  allocate (character(len=500) :: answer_sheet)
  answer_sheet = "answer.log"
  num_trails = 2
  selected = 0
  list_solved = .false.

  i = 1
  do while (i <= argument_counts)
    !> If a single problem is selected
    argument = trim(adjustl(arguments(i)))
    keywords = [character(len=500) :: &
      & "PROBLEM", "Problem", "problem", &
      & "PROB", "Prob", "prob", "P", "p"]
    if (problem_found(argument, keywords, selected)) then
      i = i + 1
      cycle
    end if

    if (argument_counts >= 2) then
      next_argument = trim(adjustl(arguments(i + 1)))
    else
      next_argument = ""
    end if

    select case (argument)
    case ("-p", "--problem")
      read (next_argument, *) selected
    case ("-t", "--trail")
      read (next_argument, *) num_trails
    case ("-d", "--data")
      data_directory = next_argument
      output_format = "('Data directory:', 1x, a)"
      write (output_unit, output_format) trim(data_directory)
    case ("-a", "--answer")
      answer_sheet = next_argument
      output_format = "('Answer sheet:', 1x, a)"
      write (output_unit, output_format) trim(answer_sheet)
    case ("-l", "--list")
      list_solved = .true.
    case ("-v", "--version")
      call get_version(messages)
      call print_characters(messages)
      return
    case ("-h", "--help")
      call get_version(messages)
      call print_characters(messages)
      call get_help(messages)
      call print_characters(messages)
      return
    case default
      error stop "[PROJECT EULER] Invalid argument."
    end select
    i = i + 2
  end do

  if (num_trails > 2) then
    output_format = "('Number of trails / problem:', 1x, i0)"
    write (output_unit, output_format) num_trails
  end if
  problems = new_problems(trim(data_directory))
  if (list_solved) then
    call list_problems(problems)
    return
  end if
  call solve_problems(problems, num_trails, selected)
  if (selected == 0) call print_answers(problems, trim(answer_sheet))
end subroutine get_arguments

end module module_driver

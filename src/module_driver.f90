module module_driver

use :: iso_fortran_env, only: int64, output_unit
use :: module_problem
implicit none

public :: get_arguments
private
character(len=:), allocatable :: help_message(:)
character(len=:), allocatable :: version_message(:)

contains

!> Print an allocatable character array.
subroutine print_characters(array)
  character(len=*), intent(in) :: array(:)
  integer(int64) :: i

  do i = 1, size(array)
    write (output_unit, "(a)") array(i)
  end do
end subroutine print_characters

!> Get version
subroutine get_version()
  version_message = [character(len=80) :: &
    & 'Project Name: PE-Fortran', &
    & 'Version: 0.4.0', 'License: MIT', &
    & 'Copyright: Copyright 2019 - 2023, Han Tang', &
    & 'Homepage: https://github.com/han190/PE-Fortran']
end subroutine get_version

!> Print help
subroutine get_help()
  help_message = [character(len=80) :: &
    & 'PE Fortran Solution', &
    & 'Arguments:', &
    & '   P<N>, PROB<N>, PROBLEM<N> Solve a single problem.', &
    & '   -v, --version             Print version.', &
    & '   -h, --help                Pop up this message.', &
    & '   -t, --trail               Number of trails.', &
    & '   -d, --data                Data directory.', &
    & '   -a, --answer              Answer sheet (output file).', &
    & '   -l, --list                List solved problems.']
end subroutine get_help

!> Print version
subroutine print_messages(message)
  character(len=*), intent(in) :: message

  select case (trim(message))
  case ("help")
    if (allocated(help_message)) deallocate (help_message)
    call get_help()
    call print_characters(help_message)
  case ("version")
    if (allocated(version_message)) deallocate (version_message)
    call get_version()
    call print_characters(version_message)
  case default
    write (output_unit, "(a, 1x, a)") "[PROJECT EULER]", trim(message)
    stop
  end select
end subroutine print_messages

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
        & call print_messages("Invalid format.")
      read (value_str, *) value
      found = .true.
      return
    end if
  end do
end function problem_found

!> Get arugment
subroutine get_arguments()
  character(len=500), allocatable :: arguments(:)
  integer :: argument_counts, i, j
  integer(int64) :: num_problems, num_trails, selected
  type(problemset_type) :: problemset
  character(len=500) :: answer_sheet, data_directory
  character(len=:), allocatable :: keywords(:), output_format
  character(len=:), allocatable :: argument, next_argument
  logical :: list_solved

  argument_counts = command_argument_count()
  if (argument_counts >= 10) &
    & call print_messages("Invalid argument count.")
  allocate (arguments(argument_counts))
  do i = 1, argument_counts
    call get_command_argument(i, arguments(i))
  end do

  !> Default values
  data_directory = "data"
  answer_sheet = "answer.log"
  num_trails = 1
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
      output_format = "('Number of trails / problem:', 1x, i0)"
      write (output_unit, output_format) num_trails
    case ("-d", "--data")
      read (next_argument, "(a)") data_directory
      output_format = "('Data directory:', 1x, a)"
      write (output_unit, output_format) trim(data_directory)
    case ("-a", "--answer")
      read (next_argument, "(a)") answer_sheet
      output_format = "('Answer sheet:', 1x, a)"
      write (output_unit, output_format) trim(answer_sheet)
    case ("-l", "--list")
      list_solved = .true.
    case ("-v", "--version")
      call print_messages("version")
      return
    case ("-h", "--help")
      call print_messages("help")
      return
    case default
      call print_messages("Invalid argument.")
    end select
    i = i + 2
  end do

  problemset = new_problemset(trim(data_directory))
  if (list_solved) then
    call list_problems(problemset)
    return
  end if
  call solve_problems(problemset, num_trails, selected)
  if (selected == 0) then
    call print_answers(problemset, trim(answer_sheet))
  end if
end subroutine get_arguments

end module module_driver

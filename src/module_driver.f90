module module_driver

use :: iso_fortran_env, only: int32, int64, output_unit
use :: module_problem
implicit none

public :: get_arguments
private
character(len=:), allocatable :: help_msgs(:)
character(len=:), allocatable :: ver_msgs(:)

interface check
  module procedure :: check_integer
  module procedure :: check_character
  module procedure :: check_message
end interface check

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
  ver_msgs = [character(len=80) :: &
    & 'Project Name: PE-Fortran', &
    & 'Version: 0.4.0', 'License: MIT', &
    & 'Copyright: Copyright 2019 - 2023, Han Tang', &
    & 'Homepage: https://github.com/han190/PE-Fortran']
end subroutine get_version

!> Print help
subroutine get_help()
  help_msgs = [character(len=80) :: &
    & 'PE Fortran Solution', &
    & 'Arguments:', &
    & '   --version            Print version.', &
    & '   --help               Pop up this message.', &
    & '   P, -p=, --problem=   (optional) Problem number. ', &
    & '   N, -n=, --trail=     (optional) Number of trails.']
end subroutine get_help

!> Print version
subroutine print_messages(message)
  character(len=*), intent(in) :: message

  select case (trim(message))
  case ("help")
    if (allocated(help_msgs)) deallocate (help_msgs)
    call get_help()
    call print_characters(help_msgs)
  case ("version")
    if (allocated(ver_msgs)) deallocate (ver_msgs)
    call get_version()
    call print_characters(ver_msgs)
  case default
    write (output_unit, "(a, 1x, a)") "[PROJECT EULER]", trim(message)
    stop
  end select
end subroutine print_messages

!> Argument check for integer
function check_integer(argument, keywords, value) result(found)
  character(len=*), intent(in) :: argument, keywords(:)
  integer(int64), intent(inout) :: value
  logical :: found
  character(len=:), allocatable :: keyword
  integer(int64) :: i

  found = .false.
  do i = 1, size(keywords)
    keyword = trim(keywords(i))
    if (index(argument, keyword) == 1) then
      read (argument(len(keyword) + 1:), *) value
      found = .true.
      return
    end if
  end do
end function check_integer

!> Argument check for integer
function check_character(argument, keywords, value) result(found)
  character(len=*), intent(in) :: argument, keywords(:)
  character(len=*), intent(inout) :: value
  logical :: found
  character(len=:), allocatable :: keyword
  integer(int64) :: i

  found = .false.
  do i = 1, size(keywords)
    keyword = trim(keywords(i))
    if (index(argument, keyword) == 1) then
      read (argument(len(keyword) + 1:), *) value
      found = .true.
      return
    end if
  end do
end function check_character

!> Argument check for integer
function check_message(argument, keywords) result(found)
  character(len=*), intent(in) :: argument, keywords(:)
  logical :: found
  character(len=:), allocatable :: keyword
  integer(int64) :: i

  found = .false.
  do i = 1, size(keywords)
    keyword = trim(keywords(i))
    if (index(argument, keyword) == 1) then
      call print_messages(keyword(3:))
      found = .true.
      return
    end if
  end do
end function check_message

!> Get arugment
subroutine get_arguments()
  character(len=500), allocatable :: arguments(:)
  integer(int32) :: argument_counts, i, j
  integer(int64) :: num_problems, num_trails, selected
  type(problem_type), allocatable :: problems(:)
  character(len=500) :: answer_sheet, data_directory
  character(len=500) :: argument, keyword, keywords(10)

  argument_counts = command_argument_count()
  if (argument_counts >= 5) &
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

  keywords = [character(len=500) :: &
    & "P", "--problem=", "T", "--trail=", "D", "--data=", &
    & "A", "--answer=", "--version", "--help"]
  do i = 1, argument_counts
    argument = trim(arguments(i))
    if ( &
      check(argument, keywords(1:2), selected) .or. &
      check(argument, keywords(3:4), num_trails) .or. &
      check(argument, keywords(5:6), data_directory) .or. &
      check(argument, keywords(7:8), answer_sheet) .or. &
      check(argument, keywords(9:10)) &
    ) cycle 
    call print_messages("Invalid argument.")
  end do

  problems = new_problems(trim(data_directory))
  call solve_problems(problems, num_trails, selected)
  if (selected == 0) call print_answers(problems, trim(answer_sheet))
end subroutine get_arguments

end module module_driver
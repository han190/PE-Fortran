module module_driver

use :: iso_fortran_env, only: int32, int64
use :: module_problem
implicit none

public :: get_arguments
private
character(len=:), allocatable :: help_messages(:)
character(len=:), allocatable :: version_messages(:)

contains

!> Print an allocatable character array.
subroutine print_characters(array)
  character(len=*), intent(in) :: array(:)
  integer(int64) :: i

  do i = 1, size(array)
    print "(a)", array(i)
  end do
end subroutine print_characters

!> Get version
subroutine get_version()
  version_messages = [character(len=80) :: &
    & 'Project Name: PE-Fortran', &
    & 'Version: 0.4.0', 'License: MIT', &
    & 'Copyright: Copyright 2019 - 2023, Han Tang', &
    & 'Homepage: https://github.com/han190/PE-Fortran']
end subroutine get_version

!> Print help
subroutine get_help()
  help_messages = [character(len=80) :: &
    & 'PE Fortran Solution', &
    & 'Arguments:', &
    & '   --version            Print version.', &
    & '   --help               Pop up this message.', &
    & '   P, -p=, --problem=   (optional) Problem number. ', &
    & '   N, -n=, --trail=     (optional) Number of trails.']
end subroutine get_help

!> Print version
subroutine print_messages(option, message)
  character(len=*), intent(in) :: option
  character(len=*), intent(in), optional :: message

  select case (trim(option))
  case ("help")
    if (allocated(help_messages)) &
      & deallocate (help_messages)
    call get_help()
    call print_characters(help_messages)
  case ("version")
    if (allocated(version_messages)) &
      & deallocate (version_messages)
    call get_version()
    call print_characters(version_messages)
  case ("error")
    if (present(message)) then
      print "(a, 1x, a)", "[ERROR]", trim(message)
    else
      print "(a)", "[ERROR]"
    end if
    stop
  end select
end subroutine print_messages

!> Get arugment
subroutine get_arguments()
  character(len=500), allocatable :: arguments(:)
  integer(int32) :: argument_counts, i, j
  integer(int64) :: num_problems, num_trails, selected
  type(problem_type), allocatable :: problems(:)
  character(len=500) :: answer_sheet, data_directory
  character(len=:), allocatable :: argument, keyword, keywords(:)

  argument_counts = command_argument_count()
  if (argument_counts >= 9) &
    & call print_messages("error", "Invalid argument count.")
  allocate (arguments(argument_counts))
  do i = 1, argument_counts
    call get_command_argument(i, arguments(i))
  end do

  data_directory = "data"
  answer_sheet = "answer.log"
  num_trails = 1
  selected = 0
  i = 1

  argument_loop: do while (i <= argument_counts)
    argument = trim(arguments(i))
    !> Problems
    keywords = [character(len=500) :: "P", "--problem=", "-p="]
    do j = 1, size(keywords)
      keyword = trim(keywords(j))
      if (index(argument, keyword) == 1) then
        read (argument(len(keyword) + 1:), *) selected
        i = i + 1
        cycle argument_loop
      end if
    end do
    !> Trails
    keywords = [character(len=500) :: "T", "--trail=", "-t="]
    do j = 1, size(keywords)
      keyword = trim(keywords(j))
      if (index(argument, keyword) == 1) then
        read (argument(len(keyword) + 1:), *) num_trails
        i = i + 1
        cycle argument_loop
      end if
    end do
    !> Version
    keywords = [character(len=500) :: "--version", "--help"]
    do j = 1, size(keywords)
      keyword = trim(keywords(j))
      if (index(argument, keyword) == 1) then
        call print_messages(keyword(3:))
        return
      end if
    end do
  end do argument_loop

  problems = new_problems(trim(data_directory))
  call solve_problems(problems, num_trails, selected)
  if (selected == 0) call print_answers(problems, trim(answer_sheet))
end subroutine get_arguments

end module module_driver
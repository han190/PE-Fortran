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
    & 'Version: 0.3.0', 'License: MIT', &
    & 'Copyright: Copyright 2019 - 2023, Han Tang', &
    & 'Homepage: https://github.com/han190/PE-Fortran']
end subroutine get_version

!> Print help
subroutine get_help()
  help_messages = [character(len=80) :: &
    & 'PE Fortran Solution', &
    & 'Arguments:', &
    & '   -v, --version             Print version.', &
    & '   -h, --help                Pop up this message.', &
    & '   P, -p, --problem          (optional) Problem number. ', &
    & '   N, -n, --number-of-trails (optional) Number of trails.']
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
  character(len=500), allocatable :: args(:)
  integer(int32) :: arg_counts, i
  integer(int64) :: num_problems, num_trails, selected
  type(problem_type), allocatable :: problems(:)
  character(len=500) :: answer_sheet, data_directory

  arg_counts = command_argument_count()
  if (arg_counts >= 9) &
    & call print_messages("error", "Invalid argument count.")
  allocate (args(arg_counts))
  do i = 1, arg_counts
    call get_command_argument(i, args(i))
  end do

  data_directory = "data"
  answer_sheet = "answer.log"
  num_trails = 1
  selected = 0
  i = 1
  do while (i <= arg_counts)
    select case (trim(args(i)))
    case ("-v", "V", "VERSION", "--version")
      call print_messages("version")
      return
    case ("-h", "H", "--help")
      call print_messages("help")
      return
    case ("-n", "N", "--number-of-trails")
      read (args(i + 1), *) num_trails
      i = i + 2
    case ("-d", "D", "--data-directory")
      read (args(i + 1), *) data_directory
      i = i + 2
    case ("-p", "P", "--problem")
      read (args(i + 1), *) selected
      i = i + 2
    case default
      call print_messages("error", "Invalid argument.")
    end select
  end do

  problems = new_problems(trim(data_directory))
  call solve_problems(problems, num_trails, selected)
  if (selected == 0) call print_answers(problems, trim(answer_sheet))
end subroutine get_arguments

end module module_driver
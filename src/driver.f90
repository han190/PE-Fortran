module driver_m

    use iso_fortran_env, only: compiler_options, compiler_version
    use problems_m
    implicit none
    private

    character(len=20), parameter :: failed = repeat(' ', 19)//'x'
    character(len=:), allocatable :: help_messages(:), version_messages(:)
    public :: get_arguments, get_levels
    character(len=:), allocatable :: level_name(:)

contains

    !> Print an allocatable character array.
    subroutine print_allocatable_character_array(character_array)
        character(len=:), allocatable, intent(in) :: character_array(:)
        integer(i32) :: i

        do i = 1, size(character_array)
            print '(a)', character_array(i)
        end do
    end subroutine print_allocatable_character_array

    !> Get version messages.
    subroutine get_version_messages()
        version_messages = &
            [character(len=80) :: &
             'Project Name: PE-Fortran', &
             'Version: 0.0.2', &
             'License: MIT', &
             'Copyright: Copyright 2019 - 2022, Han Tang', &
             'Homepage: https://github.com/han190/PE-Fortran', &
             ' ']
    end subroutine get_version_messages

    !> Print version messages.
    subroutine print_version_messages()
        call get_version_messages()
        call print_allocatable_character_array(version_messages)
    end subroutine print_version_messages

    !> Get help messages.
    subroutine get_help_messages()
        help_messages = &
            [character(len=80) :: &
             'PE Fortran Solution', &
             'Arguments:', &
             '   -v, --version          Print version.', &
             '   -h, --help             Pop up this message.', &
             '   -a N, --all N          Compute problem 1 through N.', &
             '   -p N, --problem N      Compute problem N. ', &
             '   -f, --fancy            (optional) Use emojis to express ', &
             '                          relative difficulties.', &
             '   -d, --data-directory   (optional) Directory of input data, ', &
             '                          default is ".".', &
             '   -n, --number-of-trails (optional) Number of trails, ', &
             '                          default is 1.', &
             'Example:', &
             '   (1) Compute problem 1 to 50, 10 trails per problem, with', &
             '       fancy style (emojis) and a specified data directory.', &
             '     $ PE-Fortran -f -a 50 -d $(realpath ./data) -n 10', &
             ' ']
    end subroutine get_help_messages

    !> Print help messages.
    subroutine print_help_messages()
        call get_help_messages()
        call print_allocatable_character_array(help_messages)
    end subroutine print_help_messages

    !> Print error messages.
    subroutine print_error_messages(msg)
        character(len=*), intent(in) :: msg

        print "(a)", "[SYNTAX ERROR] "//trim(msg)
        call print_help_messages()
        stop
    end subroutine print_error_messages

    !> Compute relative difficulty level.
    pure function level(x, xstr, x_min, x_max) result(ret)
        real, intent(in) :: x, x_min, x_max
        character(len=20), intent(in) :: xstr
        integer :: ret
        real :: norm
        integer :: i

        if (xstr == failed) then
            ret = 7
            return
        end if

        norm = (x - x_min)/(x_max - x_min)
        do i = 1, 5
            if (norm >= 10.**(-i)) then
                ret = i
                return
            end if
        end do
        ret = 6
    end function level

    !> Get difficulty names.
    subroutine get_level_names(fancy)
        logical, intent(in) :: fancy

        if (fancy) then
            level_name = &
                [character(len=25) :: ":smiling_imp:", ":frowning_face:", &
                 ":slightly_frowning_face:", ":confused:", ":neutral_face:", &
                 ":slightly_smiling_face:", ":construction:"]
        else
            level_name = &
                [character(len=25) :: "_lvl1_", "_lvl2_", "_lvl3_", &
                 "_lvl4_", "_lvl5_", "_lvl6_", "_unfinished_"]
        end if
    end subroutine get_level_names

    !> Get 'levels' for all problems.
    subroutine get_levels(x, xstr, levels, fancy)
        real, intent(in) :: x(:)
        character(len=20), intent(in) :: xstr(:)
        character(len=25), intent(out) :: levels(size(x))
        logical, intent(in) :: fancy
        integer :: i

        call get_level_names(fancy)
        associate (min_ => (minval(x)), max_ => (maxval(x)))
            levels = "" ! Initilization
            do i = 1, size(x)
                levels(i) = level_name(level(x(i), xstr(i), min_, max_))
            end do
        end associate
    end subroutine get_levels

    !> Get answers from problem 1 to problem x.
    subroutine get_answers(number_of_problems, number_of_trails, answer, tspan)
        integer(i32), intent(in) :: number_of_problems, number_of_trails
        character(len=20), allocatable, intent(out) :: answer(:)
        real(sp), allocatable, intent(out) :: tspan(:)
        type(problem_t), allocatable :: problem(:)
        real(sp) :: t_f, t_i
        integer(i32) :: i, j

        call initialize_problems(problem)
        allocate (answer(number_of_problems), tspan(number_of_problems))
        tspan = 0.

        do j = 1, number_of_trails
            do i = 1, number_of_problems
                call cpu_time(t_i)
                answer(i) = problem(i)%answer()
                call cpu_time(t_f)
                if (answer(i) == failed) cycle
                tspan(i) = tspan(i) + (t_f - t_i)
            end do
        end do

        tspan = tspan/number_of_trails
    end subroutine get_answers

    !> Get answer of problem x.
    subroutine get_answer(problem_number, answer, time_span)
        integer(i32), intent(in) :: problem_number
        character(len=20), intent(out) :: answer
        real(sp), intent(out) :: time_span
        type(problem_t), allocatable :: problem(:)
        real(sp) :: time_final, time_initial

        call initialize_problems(problem)
        time_span = 0.
        call cpu_time(time_initial)
        answer = problem(problem_number)%answer()
        call cpu_time(time_final)
        if (answer /= failed) then
            time_span = time_final - time_initial
        else
            print "(a)", "[ERROR] The author hasn't finish this problem yet!"
            stop
        end if
    end subroutine get_answer

    !> Print answers from problem 1 to x.
    subroutine print_answers(number_of_problems, number_of_trails, fancy_style)
        integer(i32), intent(in) :: number_of_problems, number_of_trails
        logical, intent(in) :: fancy_style
        character(len=20), allocatable :: answer(:)
        real(sp), allocatable :: tspan(:)
        real(sp) :: tsum, nslv
        character(len=7), parameter :: md_table = "|:"//repeat('-', 4)//":"
        character(len=100) :: fmt
        integer(i32), parameter :: iunit = 1120
        character(len=25), allocatable :: levels(:)
        integer(i32) :: i

        call get_answers(number_of_problems, number_of_trails, answer, tspan)
        tsum = sum(tspan, dim=1)
        nslv = real(count(answer /= failed, dim=1))
        allocate (levels(size(tspan)))
        call get_levels(tspan/(tsum/size(tspan)), answer, levels, fancy_style)

        open (unit=iunit, file='ANSWER.md')
        write (iunit, '(a)') '# Fortran PE Solutions'//new_line('a')
        write (iunit, '(a)') '## Summary'//new_line('a')
        write (iunit, '(a)') '|Benchmarks|Results|'
        write (iunit, '(a)') repeat(md_table, 2)//'|'
        write (iunit, "('|Problems solved|', i4, '|')") int(nslv)
        write (iunit, "('|Total time|', f9.2, '(s)|')") tsum*number_of_trails
        write (iunit, "('|Trails per problem|', i4, '|')") number_of_trails
        write (iunit, "('|Time spent per trail|', f9.2, '(s)|')") tsum
        write (iunit, "('|Time spent per problem|', f9.2, '(s)|')") tsum/nslv
        write (iunit, '(a)') new_line('a')//'## Answers'//new_line('a')
        write (iunit, '(a)') '|Prob|Answer|Tspan(s)|Difficulty|'
        write (iunit, '(a)') repeat(md_table, 4)//'|'
        fmt = "('|', i6, '|', a20, '|', f10.6, '|', a25, '|')"
        do i = 1, size(tspan)
            write (iunit, trim(fmt)) i, answer(i), tspan(i), levels(i)
        end do
        close (iunit)

        write (*, "('PE Fortran Solutions')")
        write (*, "(26('-'), 1x, 20('-'))")
        fmt = "('Problems solved/tried:', t27, 1x, i15.4, '/', i4.4)"
        write (*, fmt) int(nslv), size(tspan)
        write (*, "('Trails:', t27, 1x, i20)") number_of_trails
        write (*, "('Total time (s):', t27, 1x, f20.2)") tsum*number_of_trails
        write (*, "('Time per trail (s):', t27, 1x, f20.2)") tsum
        write (*, "('Time per problem (s):', t27, 1x, f20.2)") tsum/nslv
    end subroutine print_answers

    !> Print answer of problem x.
    subroutine print_answer(problem_number)
        integer(i32), intent(in) :: problem_number
        character(len=20) :: answer
        real(sp) :: time_span

        call get_answer(problem_number, answer, time_span)
        write (*, "(26('-'), 1x, 20('-'))")
        write (*, "('PE Fortran Solution')")
        write (*, "('Problem Number:', t27, 1x, i20)") problem_number
        write (*, "('Problem Answer:', t27, 1x, a20)") trim(answer)
        write (*, "('Total time spent (s):', t27, 1x, f20.10)") time_span
    end subroutine print_answer

    !> Get arguments from the command line and calculate problems.
    subroutine get_arguments()
        character(len=500), allocatable :: arguments(:)
        integer(i32) :: argument_count, idx, compute_style
        integer(i32) :: number_of_problems, number_of_trails
        logical :: fancy
        character(len=500) :: path_
        character(len=*), parameter :: INVALID = "Invalid syntax!"

        argument_count = command_argument_count()
        if (argument_count >= 9 .or. argument_count < 1) then
            call print_error_messages(INVALID)
        end if

        allocate (arguments(argument_count))
        do idx = 1, argument_count
            call get_command_argument(idx, arguments(idx))
        end do

        compute_style = 0
        fancy = .false.
        number_of_trails = 1
        data_path = "." ! declared in constant_m
        idx = 1

        do while (idx <= argument_count)
            select case (trim(arguments(idx)))
            case ("-h", "--help")
                call print_help_messages()
                return
            case ("-v", "--version")
                call print_version_messages()
                return
            case ("-a", "--all")
                read (arguments(idx + 1), *) number_of_problems
                compute_style = 2
                idx = idx + 2
            case ("-p", "--problem")
                read (arguments(idx + 1), *) number_of_problems
                compute_style = 1
                idx = idx + 2
            case ("-f", "--fancy")
                fancy = .true.
                idx = idx + 1
            case ("-d", "--data-directory")
                path_ = arguments(idx + 1)
                data_path = trim(path_)
                idx = idx + 2
            case ("-n", "--number-of-trails")
                read (arguments(idx + 1), *) number_of_trails
                idx = idx + 2
            case default
                call print_error_messages(INVALID)
            end select
        end do

        select case (compute_style)
        case (1)
            call print_answer(number_of_problems)
        case (2)
            call print_answers(number_of_problems, number_of_trails, fancy)
        case default
            call print_error_messages(INVALID)
        end select
    end subroutine get_arguments

end module driver_m

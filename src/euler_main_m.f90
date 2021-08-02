module euler_main_m

    use iso_fortran_env, only: compiler_options, compiler_version
    use euler_prob_api_m
    implicit none
    private

    character(len=20), parameter :: failed = repeat(' ', 19)//'x'
    character(len=:), allocatable :: help_messages(:), version_messages(:)
    public :: get_arguments, get_levels

contains

    !> Print an allocatable character array.
    subroutine print_allocatable_character_array(character_array)
        character(len=:), allocatable, intent(in) :: character_array(:)
        integer :: i

        do i = 1, size(character_array)
            print '(a)', character_array(i)
        end do
    end subroutine print_allocatable_character_array

    !> Get version messages.
    subroutine get_version_messages()
        version_messages = &
            [character(len=80) :: &
             'Project Name: PE-Fortran', &
             'Version: 0.0.1', &
             'License: MIT', &
             'Copyright: Copyright 2019 - 2021, Han Tang', &
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
             '   -f, --fancy            (optional) Use emojis to express ', &
             '                          relative difficulties.', &
             '   -a N, --all N          Compute problem 1 through N.', &
             '   -p N, --problem N      Compute problem N.', &
             ' ', &
             'Usage:', &
             '   (1) Compute problem 1 through 50:', &
             '       PE-Fortran --all 50', &
             '   (2) Compute problem 1 through 50 with emoji output:', &
             '       PE-Fortran --fancy --all 50', &
             '   (3) Compute problem 50:', &
             '       PE-Fortran --problem 50', &
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

    !> Get 'levels' for all problems.
    !>
    !> Here we define a concept called __relative difficulty__
    !>
    !> * First of all we compute time span per problem and store them into
    !> an array, for example `real, intent(in) :: x(:)`.
    !> * Then we normalize each time span into a number between 0 and 1.
    !> For example, `norm = (x(i) - minval(x))/(maxval(x) - minval(x))`.
    !> * We catagorize the result into 5 different levels, 1 being the highest
    !> level and 5 being the lowest level. If the argument 
    !>`fancy_style = .true.`, the levels are expressed using emojis. 
    subroutine get_levels(x, levels, fancy_style)
        real, intent(in) :: x(:)
        character(len=25), intent(out) :: levels(size(x))
        logical, intent(in) :: fancy_style
        real :: norm, min_x, max_x
        integer :: i, j
        character(len=:), allocatable :: level_names(:)

        if (fancy_style) then
            level_names = &
                [character(len=25) :: &
                 ":smiling_imp:", &
                 ":frowning_face:", &
                 ":slightly_frowning_face:", &
                 ":confused:", &
                 ":neutral_face:", ""]
        else
            level_names = &
                [character(len=25) :: &
                 "_Lv1_", "_Lv2_", "_Lv3_", "_Lv4_", "_Lv5_", ""]
        end if

        min_x = minval(x)
        max_x = maxval(x)
        levels = "" ! Initilization

        outer: do i = 1, size(x)
            norm = (x(i) - min_x)/(max_x - min_x)
            inner: do j = 5, 1, -1
                if (norm >= 10.**(-j) .and. norm <= 10.**(-j + 1)) then
                    levels(i) = trim(level_names(j))
                    exit inner
                end if
            end do inner
        end do outer

        if (fancy_style) then
            levels(maxloc(x)) = ":skull:"
        else
            levels(maxloc(x)) = "_MAX_"
        end if

    end subroutine get_levels

    !> Get answers from problem 1 to problem x.
    subroutine get_answers(problem_numbers, answer, time_span)
        integer, intent(in) :: problem_numbers
        character(len=20), allocatable, intent(out) :: answer(:)
        real, allocatable, intent(out) :: time_span(:)
        type(euler_probs_t), allocatable :: euler_problem(:)
        real :: t_f, t_i
        integer :: i

        call euler_init(euler_problem)
        allocate (answer(problem_numbers), time_span(problem_numbers))
        time_span = 0.
        do i = 1, problem_numbers
            call cpu_time(t_i)
            answer(i) = euler_problem(i)%answer()
            call cpu_time(t_f)
            if (answer(i) /= failed) time_span(i) = t_f - t_i
        end do
    end subroutine get_answers

    !> Get answer of problem x.
    subroutine get_answer(problem_number, answer, time_span)
        integer, intent(in) :: problem_number
        character(len=20), intent(out) :: answer
        real, intent(out) :: time_span
        type(euler_probs_t), allocatable :: euler_problem(:)
        real :: time_final, time_initial

        call euler_init(euler_problem)
        time_span = 0.
        call cpu_time(time_initial)
        answer = euler_problem(problem_number)%answer()
        call cpu_time(time_final)
        if (answer /= failed) then
            time_span = time_final - time_initial
        else
            print "(a)", "[ERROR] The author hasn't finish this problem yet!"
            stop
        end if
    end subroutine get_answer

    !> Print answers from problem 1 to x.
    subroutine print_answers(problem_numbers, ext, fancy_style)
        integer, intent(in) :: problem_numbers
        character(len=*), intent(in) :: ext
        logical, optional, intent(in) :: fancy_style
        character(len=20), allocatable :: answer(:)
        real, allocatable :: tspan(:)
        real :: tsum, nslv
        character(len=7), parameter :: md_table = "|:"//repeat('-', 4)//":"
        character(len=100) :: fmt
        integer, parameter :: iunit = 1120
        character(len=25), allocatable :: levels(:)
        integer :: i
        logical :: is_fancy

        if (present(fancy_style)) then
            is_fancy = fancy_style
        else
            is_fancy = .false.
        end if

        call get_answers(problem_numbers, answer, tspan)
        tsum = sum(tspan, dim=1)
        nslv = real(count(answer /= failed, dim=1))
        allocate (levels(size(tspan)))
        call get_levels(tspan/(tsum/size(tspan)), levels, fancy_style)

        select case (ext)
        case ('markdown')
            open (unit=iunit, file='ANSWER.md')
            write (iunit, '(a)') '# Fortran PE Solutions'//new_line('a')
            write (iunit, '(a)') '## Summary'//new_line('a')
            write (iunit, '(a)') '|Benchmarks|Results|'
            write (iunit, '(a)') repeat(md_table, 2)//'|'
            write (iunit, "('|Problems solved|', i4, '|')") int(nslv)
            write (iunit, "('|Time spent|', f9.2, '(s)|')") tsum
            write (iunit, "('|Time spent/problem|', f9.2, '(s)|')") tsum/nslv
            write (iunit, '(a)') new_line('a')//'## Answers'//new_line('a')
            write (iunit, '(a)') '|Prob|Answer|Tspan(s)|Difficulty|'
            write (iunit, '(a)') repeat(md_table, 4)//'|'

            fmt = "('|', i6, '|', a20, '|', f10.6, '|', a25, '|')"
            do i = 1, size(tspan)
                write (iunit, trim(fmt)) i, answer(i), tspan(i), levels(i)
            end do
            close (iunit)
        case ('text')
            open (unit=iunit, file='ANSWER.txt')
            write (iunit, '(a)') 'Fortran PE Solutions'
            write (iunit, '(a)') new_line('a')//'Summary'
            write (iunit, '(a)') '-------'
            write (iunit, "('Problems solved   ', i9)") int(nslv)
            write (iunit, "('Time spent        ', f9.2, '(s)')") tsum
            write (iunit, "('Time spent/problem', f9.2, '(s)')") tsum/nslv
            write (iunit, '(a)') new_line('a')//'Answers'
            write (iunit, '(a)') '-------'
            fmt = "(a6, a20, a10, a13)"
            write (iunit, trim(fmt)) 'Prob', 'Answer', 'Tspan(s)', 'Difficulty'

            fmt = "(i6, a20, f10.6, a13)"
            do i = 1, size(tspan)
                write (iunit, trim(fmt)) i, answer(i), tspan(i), trim(levels(i))
            end do
            close (iunit)
        case default
            error stop 'File extension not supported.'
        end select

        write (*, "(26('-'), 1x, 20('-'))")
        write (*, "('PE Fortran Solutions')")
        fmt = "('Problems solved/tried:', t27, 1x, i15.4, '/', i4.4)"
        write (*, fmt) int(nslv), size(tspan)
        write (*, "('Total time spent (s):', t27, 1x, f20.2)") tsum
        write (*, "('Time spent/problem (s):', t27, 1x, f20.2)") tsum/nslv
    end subroutine print_answers

    !> Print answer of problem x.
    subroutine print_answer(problem_number, ext)
        integer, intent(in) :: problem_number
        character(len=*), intent(in) :: ext
        character(len=20) :: answer
        real :: time_span

        call get_answer(problem_number, answer, time_span)
        write (*, "(26('-'), 1x, 20('-'))")
        write (*, "('PE Fortran Solution')")
        write (*, "('Problem Number:', t27, 1x, i20)") problem_number
        write (*, "('Problem Answer:', t27, 1x, a20)") trim(answer)
        write (*, "('Total time spent (s):', t27, 1x, f20.10)") time_span
    end subroutine print_answer

    !> Get arguments from the command line and calculate problems.
    subroutine get_arguments()
        character(len=100), allocatable :: arguments(:)
        integer :: argument_count, idx, problem_number
        logical :: compute_all, compute_single, is_fancy

        argument_count = command_argument_count()
        if (argument_count >= 5 .or. argument_count < 1) then
            call print_error_messages("Invalid argument count!")
        end if

        allocate (arguments(argument_count))
        do idx = 1, argument_count
            call get_command_argument(idx, arguments(idx))
        end do

        if (argument_count == 1) then
            select case (trim(arguments(1)))
            case ("-h", "--help")
                call print_help_messages()
                return
            case ("-v", "--version")
                call print_version_messages()
                return
            case default
                call print_error_messages("Invalid argument syntax!")
            end select
        else if (argument_count >= 2) then
            compute_single = .false.
            compute_all = .false.
            is_fancy = .false.

            idx = 1
            do while (idx <= argument_count)
                select case (trim(arguments(idx)))
                case ("-a", "--all")
                    read (arguments(idx + 1), *) problem_number
                    compute_all = .true.
                    idx = idx + 2
                case ("-p", "--problem")
                    read (arguments(idx + 1), *) problem_number
                    compute_single = .true.
                    idx = idx + 2
                case ("-f", "--fancy")
                    is_fancy = .true.
                    idx = idx + 1
                case default
                    call print_error_messages("Invalid argument syntax!")
                end select
            end do
        else
            call print_error_messages("Invalid argument count!")
        end if

        if (compute_single) then
            call print_answer(problem_number, "markdown")
        else if (compute_all) then
            call print_answers(problem_number, "markdown", is_fancy)
        else
            call print_error_messages("Invalid argument syntax!")
        end if
    end subroutine get_arguments

end module euler_main_m

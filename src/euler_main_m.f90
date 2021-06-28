module euler_main_m

    use iso_fortran_env, only: compiler_options, compiler_version
    use euler_prob_api_m
    implicit none
    private

    character(len=20), parameter :: failed = repeat(' ', 19)//'x'
    character(len=:), allocatable :: help_messages(:)
    public :: get_arguments

contains

    subroutine get_version()
        print '(a)', 'PE Fortran Solution: Version 0.0.1'
    end subroutine get_version

    subroutine get_help_messages()
        help_messages = &
            [character(len=80) :: &
             'PE Fortran Solution', &
             'Arguments:', &
             '   -v, or --version,                  Print version.', &
             '   -a N, or --all N,                  Compute problem 1 to N.', &
             '   -p N, or --problem N,              Compute problem N.', &
             '   -h, --help                         Pop up this message.', &
             ' ', &
             'Usage:', &
             '   (1) Compute problem 1 to 50:', &
             '       PE-Fortran -a 50', &
             '   (2) Compute problem 50:', &
             '       PE-Fortran -n 50', &
             ' ']
    end subroutine get_help_messages

    subroutine print_help_messages()
        integer :: i

        call get_help_messages()
        do i = 1, size(help_messages)
            print '(a)', help_messages(i)
        end do
    end subroutine print_help_messages

    subroutine print_error_msg(msg)
        character(len=*), intent(in) :: msg

        print "(a)", "[SYNTAX ERROR] "//trim(msg)
        call print_help_messages()
        stop
    end subroutine print_error_msg

    subroutine get_levels(x, levels)
        real, intent(in) :: x(:)
        character(len=25), intent(out) :: levels(size(x))
        real :: norm, min_x, max_x
        integer :: i

        min_x = minval(x)
        max_x = maxval(x)

        do i = 1, size(x)
            norm = (x(i) - min_x)/(max_x - min_x)
            if (norm >= 0. .and. norm < 10.**(-5)) then
                levels(i) = ''
            else if (norm >= 10.**(-5) .and. norm < 10.**(-4)) then
                levels(i) = '_Lv1_'
            else if (norm >= 10.**(-4) .and. norm < 10.**(-3)) then
                levels(i) = '_Lv2_'
            else if (norm >= 10.**(-3) .and. norm < 10.**(-2)) then
                levels(i) = '_Lv3_'
            else if (norm >= 10.**(-2) .and. norm < 10.**(-1)) then
                levels(i) = '_Lv4_'
            else if (norm >= 10.**(-1) .and. norm <= 10.**(0)) then
                levels(i) = '_LV5_'
            end if
        end do
    end subroutine get_levels

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

    subroutine print_answers(problem_numbers, ext)
        integer, intent(in) :: problem_numbers
        character(len=*), intent(in) :: ext
        character(len=20), allocatable :: answer(:)
        real, allocatable :: tspan(:)
        real :: tsum, nslv
        character(len=7), parameter :: md_table = "|:"//repeat('-', 4)//":"
        character(len=100) :: fmt
        integer, parameter :: iunit = 1120
        character(len=25), allocatable :: levels(:)
        integer :: i

        call get_answers(problem_numbers, answer, tspan)
        tsum = sum(tspan, dim=1)
        nslv = real(count(answer /= failed, dim=1))
        allocate (levels(size(tspan)))
        call get_levels(tspan/(tsum/size(tspan)), levels)

        select case (ext)
        case ('markdown')
            open (unit=iunit, file='answer.md')
            write (iunit, '(a)') '# Fortran PE Solutions'//new_line('a')
            write (iunit, '(a)') new_line('a')//'## Summary'//new_line('a')
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
            open (unit=iunit, file='answer.txt')
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

    subroutine get_arguments()
        character(len=100), allocatable :: arguments(:)
        integer :: argument_count, idx, problem_number
        logical :: compute_all, compute_single

        argument_count = command_argument_count()
        if (argument_count >= 5 .or. argument_count < 1) then
            call print_error_msg("Invalid argument count!")
        end if

        allocate(arguments(argument_count))
        do idx = 1, argument_count
            call get_command_argument(idx, arguments(idx))
        end do

        if (argument_count == 1) then
            select case (trim(arguments(1)))
            case ("-h", "--help")
                call print_help_messages()
                return
            case ("-v", "--version")
                call get_version()
                return
            case default
                call print_error_msg("Invalid argument syntax!")
            end select
        else if (argument_count == 2) then
            compute_single = .false.
            compute_all = .false.

            idx = 1
            select case (trim(arguments(idx)))
            case ("-a", "--all")
                read(arguments(idx + 1), *) problem_number
                compute_all = .true.
            case ("-p", "--problem")
                read(arguments(idx + 1), *) problem_number
                compute_single = .true.
            case default
                call print_error_msg("Invalid argument syntax!")
            end select
        else
            call print_error_msg("Invalid argument count!")
        end if

        if (compute_single) then
            call print_answer(problem_number, "markdown")
        else if (compute_all) then
            call print_answers(problem_number, "markdown")
        else
            call print_error_msg("Invalid argument syntax!")
        end if

    contains

        ! Inspired by fpm source code
        logical function is_windows()
            character(len=32) :: val
            integer :: length, rc

            is_windows = .false.
            call get_environment_variable('OS', val, length, rc)
            if (rc == 0 .and. length > 0 .and. &
                index(val, 'Windows_NT') > 0) then
                is_windows = .true.
            end if
        end function is_windows

        function last_character(str) result(ret)
            character(len=*), intent(in) :: str
            character(len=1) :: ret

            ret = str(len(str):len(str))
        end function last_character
    end subroutine get_arguments

end module euler_main_m

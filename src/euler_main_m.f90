module euler_main_m

    use iso_fortran_env, only: compiler_options, compiler_version
    use euler_prob_api_m
    implicit none

    character(len=1), parameter :: space = ' ', dash = '-'
    character(len=4), parameter :: tab = repeat(space, 4)
    character(len=8), parameter :: tab2 = repeat(space, 8)
    character(len=20), parameter :: failed = repeat(space, 19)//'x'

contains

    subroutine get_version()
        print '(a)', 'PE Fortran Solution: Version 0.0.1'
    end subroutine get_version

    subroutine get_help()
        character(len=:), allocatable :: fmt

        fmt = '(t4, a, t40, a)'
        print '(a)', 'PE Fortran Solution'
        print '(a)', 'Arguments:'
        print fmt, '-v, or --version', 'Version.'
        print fmt, '-a N, or --all N', 'Compute problem 1 to N.'
        print fmt, '-n N, or --problem-number N', 'Compute problem N.'
        print fmt, '-d /path/to/data/, or '
        print fmt, ' --data-directory /path/to/data/', 'Path to data.'
        print fmt, '-h, --help', 'Pop up this message.'//new_line('a')
        print '(a)', 'Usage:'
        print '(a)', '(1) Compute problem 1 to 50:'
        print fmt, './PE-Fortran -a 50 -d /path/to/data/'
        print '(a)', '(2) Compute problem 50:'
        print fmt, './PE-Fortran -n 50 -d /path/to/data/'//new_line('a')
        fmt = '(t1, a, t4, a)'
        print '(a)', 'Tips:'
        print fmt, '*', 'All the data required are stored in the directory:'
        print fmt, ''
        print fmt, '', '/path/to/the/project/PE-Fortran/data/'
        print fmt, ''
        print fmt, '', "The argument '--data-directory' requires an absolute"
        print fmt, '', 'path but you can use'
        print fmt, '', './PE-Fortran -n 50 -d $(realpath /relative/data/path/)'
    end subroutine get_help

    subroutine error_msg(msg)
        character(len=*), intent(in) :: msg

        print "(a)", "[SYNTAX ERROR] "//trim(msg)
        call get_help()
        stop
    end subroutine error_msg

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
            error stop "[ERROR] Problem isn't calculated."
        end if
    end subroutine get_answer

    subroutine print_answers(problem_numbers, ext)
        integer, intent(in) :: problem_numbers
        character(len=*), intent(in) :: ext
        character(len=20), allocatable :: answer(:)
        real, allocatable :: tspan(:)
        real :: tsum, nslv
        character(len=7), parameter :: md_table = "|:"//repeat(dash, 4)//":"
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

end module euler_main_m

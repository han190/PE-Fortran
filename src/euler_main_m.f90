module euler_main_m

    use iso_fortran_env, only: compiler_options, compiler_version
    use euler_prob_api_m
    implicit none

    character(len=1), parameter :: space = ' ', dash = '-'
    character(len=4), parameter :: tab = repeat(space, 4)
    character(len=8), parameter :: tab2 = repeat(space, 8)
    character(len=20), parameter :: failed = repeat(space, 19)//'x'

contains

    subroutine get_help()
        print '(a)', 'PE Fortran Solution'
        print '(a)', ''
        print '(t4, a, t24, a)', '-ca, --compute-all', 'Compute all problems.'
        print '(t4, a, t24, a)', '-h,  --help', 'Pop up this message.'
    end subroutine get_help

    subroutine error_msg()
        print "(a)", "[SYNTAX ERROR]"
        call get_help()
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

    subroutine get_answers(answer, time_span)
        character(len=20), allocatable, intent(out) :: answer(:)
        real, allocatable, intent(out) :: time_span(:)
        type(euler_probs_t), allocatable :: euler_problem(:)
        real :: t_f, t_i
        integer :: i

        call euler_init(euler_problem)
        associate (NUM_PROBLEMS => size(euler_problem))
            allocate (answer(NUM_PROBLEMS), time_span(NUM_PROBLEMS))
            time_span = 0.
            do i = 1, NUM_PROBLEMS
                call cpu_time(t_i)
                answer(i) = euler_problem(i)%answer()
                call cpu_time(t_f)
                if (answer(i) /= failed) time_span(i) = t_f - t_i
            end do
        end associate
    end subroutine get_answers

    subroutine print_answer(ext)
        character(len=*), intent(in) :: ext
        character(len=20), allocatable :: answer(:)
        real, allocatable :: tspan(:)
        real :: tsum, nslv
        character(len=7), parameter :: md_table = "|:"//repeat(dash, 4)//":"
        character(len=100) :: fmt
        integer :: iunit, i
        character(len=25), allocatable :: levels(:)

        call get_answers(answer, tspan)
        tsum = sum(tspan, dim=1)
        nslv = real(count(answer /= failed, dim=1))
        allocate (levels(size(tspan)))
        call get_levels(tspan/(tsum/size(tspan)), levels)

        select case (ext)
        case ('markdown')
            iunit = 1120
            open (unit=iunit, file='ANSWER.md')
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
            print_all_answers: do i = 1, size(tspan)
                write (iunit, trim(fmt)) i, answer(i), tspan(i), levels(i)
            end do print_all_answers
            close (iunit)
        case default
            error stop 'File extension not supported.'
        end select

        write (*, "('PE Fortran Solutions')")
        write (*, "(26('-'), 1x, 9('-'))")
        write (*, "('Problems solved:', t27, 1x, i9)") int(nslv)
        write (*, "('Total time spent (s):', t27, 1x, f9.2)") tsum
        write (*, "('Time spent per problem (s):', t27, 1x, f9.2)") tsum/nslv
    end subroutine print_answer

end module euler_main_m

module euler_main_m

    use iso_fortran_env, only: compiler_options, compiler_version
    use euler_prob_api_m
    implicit none

    character(len=1), parameter :: space = ' ', dash = '-'
    character(len=4), parameter :: tab = repeat(space, 4)
    character(len=8), parameter :: tab2 = repeat(space, 8)

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

    function compute_diff(t_, max_, min_) result(ret)
        real, intent(in) :: t_, max_, min_
        character(len=25) :: ret
        real :: diff

        ret = ''
        diff = (t_ - min_)/(max_ - min_)
        if (diff >= 0. .and. diff < 10.**(-5)) then
            ret = ''
        else if (diff >= 10.**(-5) .and. diff < 10.**(-4)) then
            ret = 'Lv1'
        else if (diff >= 10.**(-4) .and. diff < 10.**(-3)) then
            ret = 'Lv2'
        else if (diff >= 10.**(-3) .and. diff < 10.**(-2)) then
            ret = 'Lv3'
        else if (diff >= 10.**(-2) .and. diff < 10.**(-1)) then
            ret = 'Lv4'
        else if (diff >= 10.**(-1) .and. diff <= 10.**(0)) then
            ret = 'Chronoeater'
        end if
    end function compute_diff

    subroutine compute_all(ans, tspan, tsum, nslv)
        character(len=20), intent(out) :: ans(nop)
        real, intent(out) :: tspan(nop), tsum, nslv
        character(len=20), parameter :: failed = repeat(space, 19)//'x'
        real :: t_f, t_i
        type(euler_probs_t) :: probs(nop)
        integer :: i

        call euler_init(probs)
        tspan = 0.; tsum = 0.; nslv = 0.
        do i = 1, nop
            call cpu_time(t_i)
            ans(i) = probs(i)%ans()
            call cpu_time(t_f)
            tspan(i) = t_f - t_i
        end do
        tsum = sum(tspan, dim=1)
        nslv = real(count(ans /= failed, dim=1))
    end subroutine compute_all

    subroutine print_answer(ext)
        character(len=*), intent(in) :: ext
        character(len=20) :: ans(nop)
        real :: tspan(nop), tsum, nslv, diff_(nop)
        character(len=7), parameter :: c_aligned = "|:"//repeat(dash, 4)//":"
        character(len=100) :: fmt, x
        integer :: iunit, i

        call compute_all(ans, tspan, tsum, nslv)
        select case (ext)
        case ('markdown')
            iunit = 1120
            open (iunit, file='ANSWER.md')
            write (iunit, '(a)') '# Fortran PE Solutions'
            write (iunit, '(a)') new_line('a')//'## Compilers'//new_line('a')
            write (iunit, '(a)') '- Compiler version: '//compiler_version()
            write (iunit, '(a)') '- Compiler options: '//compiler_options()
            write (iunit, '(a)') new_line('a')//'## Summary'//new_line('a')
            write (iunit, '(a)') '|Benchmarks|Results|'
            write (iunit, '(a)') repeat(c_aligned, 2)//'|'
            write (iunit, "('|Problems solved|', i4, '|')") int(nslv)
            write (iunit, "('|Time spent|', f9.2, '(s)|')") tsum
            write (iunit, "('|Time spent per problem|', f9.2, '(s)|')") &
                tsum/nslv
            write (iunit, '(a)') new_line('a')//'## Relative Difficulty'// &
                new_line('a')
            write (iunit, '(a)') '- Relative Difficulty of a problem = '// &
                ' Normalize [ Tspan / ( Tsum / Nprob ) ]'//new_line('a')
            write (iunit, '(a)') '|Level 0|Level 1|Level 2|'// &
                'Level 3|Level 4|Level 5|'
            write (iunit, '(a)') repeat(c_aligned, 6)//'|'
            write (iunit, '(a)') '|~10<sup>-6<sup/>|~10<sup>-5<sup/>|'// &
                '~10<sup>-4<sup/>|~10<sup>-3<sup/>|~10<sup>-2<sup/>|'// &
                '~10<sup>-1<sup/>|'
            write (iunit, '(a)') '||Lv1|Lv2|Lv3|Lv4|Chronoeater|'
            write (iunit, '(a)') new_line('a')//'## Answers'//new_line('a')
            write (iunit, '(a)') &
                '|Prob|Answer|Tspan(s)|Relative Difficulty|'
            write (iunit, '(a)') repeat(c_aligned, 4)//'|'

            fmt = "('|', i6, '|', a20, '|', f10.6, '|', a25, '|')"
            diff_ = tspan/(tsum/nop)
            print_all_answers: do i = 1, nop
                x = compute_diff(diff_(i), maxval(diff_), minval(diff_))
                write (iunit, trim(fmt)) i, ans(i), tspan(i), trim(x)
            end do print_all_answers
            close (iunit)
        case default
            error stop 'File extension not supported.'
        end select

        write (*, "('PE Fortran Solutions')")
        write (*, "('Problems solved:', t20, i9)") int(nslv)
        write (*, "('Total time spent:', t20, f9.2, '(s)')") tsum
        write (*, "('Time spent per problem:', t20, f9.2, '(s)')") tsum/nslv
    end subroutine print_answer

end module euler_main_m

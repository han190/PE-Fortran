program main

    use iso_fortran_env, only: compiler_version, compiler_options
    use euler_prob_api_m
    implicit none

    integer :: index_
    character(len=100) :: arg_val(1:2)

    if (command_argument_count() > 2) then
        print "(a)", "SYNTAX ERROR: -h or --help for further information."
        stop
    end if

    index_ = 1
    read_argument_loop: do
        if (len_trim(arg_val(index_)) == 0 .or. index_ >= 2) then
            exit read_argument_loop
        end if

        call get_command_argument(index_, arg_val(index_))
        index_ = index_ + 1
    end do read_argument_loop

    select case (trim(arg_val(1)))
    case ("-h", "--help")
        call get_help()
        stop
    case ("-ca", "--compute-all")
        call compute_all("ANSWER.md")
    case default
        print "(a)", "SYNTAX ERROR: -h or --help for further information."
        stop
    end select

contains

    subroutine get_help()
        print "(a)", "Project Euler with Modern Fortran"
        print "(a)", "Syntax:"
        print "(a)", "./pe-fortran -h/--help"
        print "(a)", "./pe-fortran -ca/--compute-all"
    end subroutine get_help

    function difficulty_bar(t_, max_, min_) result(ret)
        real, intent(in) :: t_, max_, min_
        character(len=25) :: ret
        real :: angry_level

        ret = ""
        angry_level = (t_ - min_)/(max_ - min_)
        if (angry_level >= 0. .and. angry_level < 10.**(-5)) then
            ret = ""
        else if (angry_level >= 10.**(-5) .and. angry_level < 10.**(-4)) then
            ret = ":neutral_face:"
        else if (angry_level >= 10.**(-4) .and. angry_level < 10.**(-3)) then
            ret = ":slightly_frowning_face:"
        else if (angry_level >= 10.**(-3) .and. angry_level < 10.**(-2)) then
            ret = ":confused:"
        else if (angry_level >= 10.**(-2) .and. angry_level < 10.**(-1)) then
            ret = ":frowning_face:"
        else if (angry_level >= 10.**(-1) .and. angry_level <= 10.**(0)) then
            ret = ":imp:"
        end if
    end function difficulty_bar

    subroutine compute_all(filename)
        character(len=*), intent(in) :: filename
        character(len=20) :: ans(nop)
        character(len=1), parameter :: space = " ", dash = "-"
        character(len=20), parameter :: failed = repeat(space, 19)//"x"
        character(len=7), parameter :: c_aligned = "|:"//repeat(dash, 4)//":"
        real :: t_f, t_i, tspan(nop), tsum = 0., nslv = 0.
        type(euler_probs_t) :: probs(nop)
        integer :: i, iunit
        character(len=100) :: fmt
        real :: rel_diff(nop)

        call euler_init(probs)
        tspan = 0.
        do i = 1, nop
            call cpu_time(t_i)
            ans(i) = probs(i)%ans()
            call cpu_time(t_f)
            tspan(i) = t_f - t_i
        end do
        tsum = sum(tspan, dim=1)
        nslv = real(count(ans /= failed, dim=1))

        iunit = 1120
        open (iunit, file=filename)
        write (iunit, "(a)") "# Fortran PE Solutions"//new_line("a")
        write (iunit, "(a)") "## Compilers"//new_line("a")
        write (iunit, "(a)") "- Compiler version: "//compiler_version()
        write (iunit, "(a)") "- Compiler options: "//compiler_options()
        write (iunit, "(a)") new_line("a")//"## Summary"//new_line("a")
        write (iunit, "(a)") "|Benchmarks|Results|"
        write (iunit, "(a)") repeat(c_aligned, 2)//"|"
        write (iunit, "('|Problems solved|', i4, '|')") int(nslv)
        write (iunit, "('|Time spent|', f9.2, '(s)|')") tsum
        write (iunit, "('|Time spent per problem|', f9.2, '(s)|')") tsum/nslv
        write (iunit, "(a)") new_line("a")//"## Relative Difficulty"// &
            new_line("a")
        write (iunit, "(a)") "Relative Difficulty of a problem = "// &
            " Normalize [ Tspan / ( Tsum / Nprob ) ]"
        write (iunit, "(a)") "(Thus RD is a real number between 0 and 1.)"
        write (iunit, "(a)") "|Level 0|Level 1|Level 2|"// &
            "Level 3|Level 4|Time<br/>Consuming!|"
        write (iunit, "(a)") repeat(c_aligned, 6)//"|"
        write (iunit, "(a)") "|~10<sup>-6<sup/>|~10<sup>-5<sup/>|"// &
            "~10<sup>-4<sup/>|~10<sup>-3<sup/>|~10<sup>-2<sup/>|"// &
            "~10<sup>-1<sup/>|"
        write (iunit, "(a)") "||:neutral_face:|:slightly_frowning_face:|"// &
            ":confused:|:frowning_face:|:imp:|"
        write (iunit, "(a)") new_line("a")//"## Answers"//new_line("a")
        write (iunit, "(a)") "|Prob|Answer|Tspan(s)|Relative<br/>Difficulty|"
        write (iunit, "(a)") repeat(c_aligned, 4)//"|"
        fmt = "('|', i6, '|', a20, '|', f10.6, '|', a25, '|')"
        rel_diff = tspan/(tsum/nop)
        print_all_answers: do i = 1, nop
            write (iunit, trim(fmt)) i, ans(i), tspan(i), &
                difficulty_bar(rel_diff(i), maxval(rel_diff), minval(rel_diff))
        end do print_all_answers
        close (iunit)

        print "(a)", "Fortran PE Solutions, Version (0.0.1) "
        print "(a)", ">>> Quick results <<<"
        fmt = "(a24, t27, i4.4, '/', i4.4)"
        print trim(fmt), "Problems solved/tried:  ", int(nslv), nop
        fmt = "(a24, t27, f6.3, a)"
        print trim(fmt), "Total time spent:       ", tsum, "(s)"
        print trim(fmt), "Time spent per problem: ", tsum/nslv, "(s)"
    end subroutine compute_all

end program main

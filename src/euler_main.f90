program main
    use iso_fortran_env, only: compiler_version
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
        write (iunit, "(a)") compiler_version()
        write (iunit, "(a)") new_line("a")//"## Summary"//new_line("a")
        write (iunit, "(a)") "|Benchmarks|Results|"
        write (iunit, "(a)") repeat(c_aligned, 2)//"|"
        write (iunit, "('|Problems solved|', i4, '|')") int(nslv)
        write (iunit, "('|Time spent|', f9.2, '(s)|')") tsum
        write (iunit, "('|Time spent per problem|', f9.2, '(s)|')") tsum/nslv
        write (iunit, "(a)") new_line("a")//"## Answers"//new_line("a")
        write (iunit, "(a)") "|Prob|Answer|Tspan(s)|T/Ttot(%)|"
        write (iunit, "(a)") repeat(c_aligned, 4)//"|"
        fmt = "('|', i6, '|', a20, '|', f10.6, '|', f9.4, '%|')"
        print_all_answers: do i = 1, nop
            write (iunit, trim(fmt)) i, ans(i), tspan(i), tspan(i)/tsum*100.
        end do print_all_answers
        close(iunit)

        print "(a)", "==================================="
        print "(a)", "       Fortran PE Solutions        "
        print "(a)", "==================================="
        print "(a)", "Quick results:"
        fmt = "(a24, t27, i4.4, '/', i4.4)"
        print trim(fmt), "Problems solved/tried:  ", int(nslv), nop
        fmt = "(a24, t27, f6.3, a)"
        print trim(fmt), "Total time spent:       ", tsum, "(s)"
        print trim(fmt), "Time spent per problem: ", tsum/nslv, "(s)"
        print "(a)", "==================================="
    end subroutine compute_all
end program main
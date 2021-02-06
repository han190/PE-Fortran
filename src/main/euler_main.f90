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

    subroutine print_logo()            
        print "(a)", " "
        print "(a)", " _____  _____       _____            _                  "
        print "(a)", "|  _  ||   __| ___ |   __| ___  ___ | |_  ___  ___  ___ "
        print "(a)", "|   __||   __||___||   __|| . ||  _||  _||  _|| .'||   |"
        print "(a)", "|__|   |_____|     |__|   |___||_|  |_|  |_|  |__,||_|_|"
        print "(a)", " "
    end subroutine print_logo

    subroutine get_help()
        print "(a)", "Project Euler with Modern Fortran"
        print "(a)", "Syntax:"
        print "(a)", "./pe-fortran -h/--help"
        print "(a)", "./pe-fortran -ca/--compute-all"
    end subroutine get_help

    function difficulty_bar(n) result(ret)
        real, intent(in) :: n
        character(len=25) :: ret
        integer :: angry_level

        ret = " "
        angry_level = int(n/(100./nop))
        select case (angry_level)
        case (0)
            ret = ""
        case (1, 2)
            ret = ":neutral_face:"
        case (3, 4)
            ret = ":slightly_frowning_face:"
        case (5, 6)
            ret = ":confused:"
        case (7, 8)
            ret = ":frowning_face:"
        case (9:)
            ret = ":imp:"
        end select
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
        write (iunit, "(a)") "Compiler: "//compiler_version()
        write (iunit, "(a)") new_line("a")//"## Summary"//new_line("a")
        write (iunit, "(a)") "|Benchmarks|Results|"
        write (iunit, "(a)") repeat(c_aligned, 2)//"|"
        write (iunit, "('|Problems solved|', i4, '|')") int(nslv)
        write (iunit, "('|Time spent|', f9.2, '(s)|')") tsum
        write (iunit, "('|Time spent per problem|', f9.2, '(s)|')") tsum/nslv
        write (iunit, "(a)") new_line("a")//"## Relative Difficulty"//&
            new_line("a")
        write (iunit, "(a)") "Relative Difficulty of a problem = "//&
            "( Time span of the problem / Time span of all problems ) / "//&
            "( 100 / Number of problems solved )"
        write (iunit, "(a)") "|<1|1~2|3~4|5~6|7~8|>9|"
        write (iunit, "(a)") repeat(c_aligned, 6)//"|"
        write (iunit, "(a)") "||:neutral_face:|:slightly_frowning_face:|"//&
            ":confused:|:frowning_face:|:imp:|"
        write (iunit, "(a)") new_line("a")//"## Answers"//new_line("a")
        write (iunit, "(a)") "|Prob|Answer|Tspan(s)|Relative<br />Difficulty|"
        write (iunit, "(a)") repeat(c_aligned, 4)//"|"
        fmt = "('|', i6, '|', a20, '|', f10.6, '|', a25, '|')"
        print_all_answers: do i = 1, nop
            write (iunit, trim(fmt)) i, ans(i), tspan(i), &
                difficulty_bar(tspan(i)/tsum*100.)
        end do print_all_answers
        close(iunit)

        call print_logo()
        print "(a)", "Fortran PE Solutions, Version (0.0.1) "
        print "(a)", ">>> Quick results <<<"
        fmt = "(a24, t27, i4.4, '/', i4.4)"
        print trim(fmt), "Problems solved/tried:  ", int(nslv), nop
        fmt = "(a24, t27, f6.3, a)"
        print trim(fmt), "Total time spent:       ", tsum, "(s)"
        print trim(fmt), "Time spent per problem: ", tsum/nslv, "(s)"
    end subroutine compute_all

end program main
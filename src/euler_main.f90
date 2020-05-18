program main 
    use iso_fortran_env, only: compiler_version
    ! use file_generator_m, only: nop
    use euler_prob_api_m
    implicit none 

    integer :: arg_num, arg_len, arg_stat
    character(len=50) :: arg_val(0:2)

    if (command_argument_count() > 4) then
        print "(a)", "SYNTAX ERROR: -h or --help for further information."
        stop
    end if

    arg_num = 0
    read_argument_loop: do
        call get_command_argument(arg_num, arg_val(arg_num), arg_len, arg_stat)
        if (len_trim(arg_val(arg_num)) == 0 .or. arg_num >= 3) then
            exit read_argument_loop
        end if
        arg_num = arg_num + 1
    end do read_argument_loop

    select case(arg_val(1))
    case("-h", "--help")
        call get_help()
        stop
    case("-c", "--compute-all-problems")
        call compute_all_problems()
    case default
        print "(a)", "SYNTAX ERROR: -h or --help for further information."
        stop
    end select

contains

    subroutine get_help()
        print "(a)", "Project Euler with Modern Fortran"
        print "(a)", "Syntax:"
        print "(a)", "./Euler -h/--help"
        print "(a)", "./Euler -c/--compute-all-problems"
    end subroutine get_help

    subroutine compute_all_problems()
        integer, parameter :: nop = 58
        character(len=20) :: ans(nop)
        character(len=1), parameter :: space = " ", dash = "-"
        character(len=20), parameter :: failed = repeat(space, 19)//"x"
        character(len=7), parameter :: c_aligned = "|:"//repeat(dash, 4)//":"
        real :: t_f, t_i, tspan(nop), tsum = 0., nslv = 0.

        type(euler_probs_t) :: probs(nop)
        integer :: i

        call euler_init(probs)

        open(1, file = "ANSWER.md")
        write (1, "(a)") "# Project Euler with Modern Fortran"//new_line("a")
        write (1, "(a)") "## Compilers"//new_line("a")
        write (1, "(a)") compiler_version()//new_line("a")
        write (1, "(a)") "## Answers and Benchmarks"//new_line("a")
        write (1, "(a)") "|Prob|Answer|Tspan(s)|T/Ttot(%)|"
        write (1, "(a)") repeat(c_aligned, 4)//"|"

        do i = 1, nop
            call cpu_time(t_i)
            ans(i) = probs(i)%ans()
            call cpu_time(t_f)
            tspan(i) = t_f - t_i
        end do

        tsum = sum(tspan, dim = 1)
        nslv = real(count(ans /= failed, dim = 1))

        do i = 1, nop
            write (1, 1120) i, ans(i), tspan(i), tspan(i) / tsum * 100.
            1120 format("|", i6, "|", a20, "|", f10.6, "|", f9.4, "%|")
        end do

        write (1, "(a)") new_line("a")//"## Summary"//new_line("a")
        write (1, "(a)") "|Benchmarks|Results|"
        write (1, "(a)") repeat(c_aligned, 2)//"|"
        write (1, 1121) int(nslv)
        1121 format("|Problems solved|", i4, "|")
        write (1, 1122) "|Total time spent|", tsum, "(s)|"
        write (1, 1122) "|Average time spent per problem|", tsum/nslv, "(s)|"
        1122 format(a, f10.6, a)

        close(1)
    end subroutine compute_all_problems

end program main 

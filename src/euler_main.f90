program main 
    use iso_fortran_env, only: compiler_version
    ! use file_generator_m, only: nop
    use euler_prob_api_m
    implicit none 

    integer, parameter :: nop = 58
    character(len=20) :: ans(nop), failed
    real :: t_f, t_i, tspan(nop), tsum = 0.
    type(euler_probs_t) :: probs(nop)
    integer :: i

    failed = '                   x'
    call euler_init(probs)

    open(47, file = 'ANSWER.md')
    write (47, "(a)") "# Project Euler with Modern Fortran"//new_line("a")
    write (47, "(a)") "## Compilers"//new_line("a")
    write (47, "(a)") compiler_version()//new_line("a")
    write (47, "(a)") "## Answers and Benchmarks"//new_line("a")
    write (47, "(a)") "|Prob  |Answer             |Tspan(s)    |Tspan/Ttot(%) |"
    write (47, "(a)") "|:----:|:-----------------:|:----------:|:------------:|"

    do i = 1, nop
        call cpu_time(t_i)
        ans(i) = probs(i)%ans()
        if ( ans(i) == '                   0') then
            ans(i) = failed
        end if
        call cpu_time(t_f)
        tspan(i) = t_f - t_i
    end do
    tsum = sum(tspan, dim = 1)

    do i = 1, nop
        write (47, 1120) "|", i, "|"//ans(i)//"|", tspan(i), &
            "|", tspan(i) / tsum * 100., "%|"
    end do

    write (47, "(a)") new_line("a")//"## Summary"//new_line("a")
    write (47, "(a)") "|Summary||"
    write (47, "(a)") "|:---|:---|"
    write (47, "(a, i4, a)") "| Problems solved  | ", &
        count(ans /= failed), " |"
    write (47, 1121) "| Total time spent | ", tsum, "(s) |"
    write (47, 1121) "| Average time spent per problem | ", &
        tsum/count(ans /= failed, dim = 1), "(s) |"

    close(47)

1120 format(a, i4, a22, f10.6, a, f8.4, a)
1121 format(a, f10.6, a)
end program main 

program main

    use euler_main_m
    use euler_utils_m, only: data_dir
    implicit none

    integer :: i, k, problem_number
    character(len=100) :: arg(4)

    ! data_dir = "/home/han/Projects/PE-Fortran/data/"
    k = command_argument_count()
    if (k >= 5 .or. k < 1) call error_msg()

    read_argument_loop: do i = 1, 4
        call get_command_argument(i, arg(i))
    end do read_argument_loop

    do i = 1, 4, 2
        if (trim(arg(i)) == "-d" .or. trim(arg(i)) == "--data-directory") &
            data_dir = trim(arg(i + 1))
    end do

    do i = 1, 4, 2
        select case (trim(arg(i)))
        case ("-h", "--help")
            call get_help()
            stop
        case ("-a", "--all")
            read (arg(i + 1), *) problem_number
            call print_answers(problem_number, 'markdown')
        case ("-v", "--version")
            call get_version()
        case ("-n", "--problem-number")
            read (arg(i + 1), *) problem_number
            call print_answer(problem_number, 'markdown')
        end select
    end do

end program main

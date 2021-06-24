program main

    use euler_main_m
    use euler_utils_m, only: data_dir
    implicit none

    integer :: i, k, problem_number
    character(len=100) :: arg(4)
    logical :: no_data_dir_specified

    k = command_argument_count()
    if (k >= 5 .or. k < 1) call error_msg("Invalid argument count!")

    read_argument_loop: do i = 1, 4
        call get_command_argument(i, arg(i))
    end do read_argument_loop

    no_data_dir_specified = .true.
    do i = 1, 4, 2
        if (trim(arg(i)) == "-d" .or. &
            trim(arg(i)) == "--data-directory") then
            data_dir = trim(arg(i + 1))
            no_data_dir_specified = .false.
        end if
    end do

    do i = 1, 4, 2
        select case (trim(arg(i)))
        case ("-h", "--help")
            call get_help()
            stop
        case ("-a", "--all")
            if (no_data_dir_specified) then
                call error_msg("Data directory not specified!")
            end if
            read (arg(i + 1), *) problem_number
            call print_answers(problem_number, 'markdown')
            exit
        case ("-v", "--version")
            call get_version()
        case ("-n", "--problem-number")
            if (no_data_dir_specified) then
                call error_msg("Data directory not specified!")
            end if
            read (arg(i + 1), *) problem_number
            call print_answer(problem_number, 'markdown')
            exit
        case default
            call error_msg("Invalid argument!")
        end select
    end do

end program main

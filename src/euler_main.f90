program main

    use euler_main_m
    implicit none

    integer :: index_
    character(len=100) :: arg_val(1:2)

    if (command_argument_count() > 2 .or. &
        command_argument_count() < 1) then
        call error_msg()
        stop
    end if

    index_ = 1
    read_argument_loop: do
        if (len_trim(arg_val(index_)) == 0 &
            .or. index_ >= 2) then
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
        call print_answer('markdown')
    case default
        call error_msg()
        stop
    end select

end program main

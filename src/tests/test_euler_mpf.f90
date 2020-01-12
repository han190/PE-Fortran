program test_euler_mpf
    use euler_mi_m
    implicit none 

    test_assignment: block 
        integer, allocatable :: arr(:)
        logical :: cond 
        character(len=10) :: func_name
        type(very_long_int_t) :: i, j

        i = '+20349587294385203457829345728349502934875239485720349587'
        arr = [ &
            2, 0, 3, 4, 9, 5, 8, 7, 2, 9, 4, &
            3, 8, 5, 2, 0, 3, 4, 5, 7, 8, 2, &
            9, 3, 4, 5, 7, 2, 8, 3, 4, 9, 5, &
            0, 2, 9, 3, 4, 8, 7, 5, 2, 3, 9, &
            4, 8, 5, 7, 2, 0, 3, 4, 9, 5, 8, 7 &
        ]
        j = arr

        func_name = 'assignment'
        cond = i == j 
        
        call if_passed(cond, func_name)
    end block test_assignment
            
contains 

    subroutine testing(test_msg)
        character(len=*), intent(in) :: test_msg 

        print '(a)', "Testing '"//test_msg//"' ..."
    end subroutine testing 

    subroutine error_stop(err_msg)
        character(len=*), intent(in) :: err_msg

        print '(a)', "Testing '"//err_msg//"' failed."
        error stop 
    end subroutine error_stop

    subroutine if_passed(logical_cond, char_func_name)
        logical, intent(in) :: logical_cond 
        character(len=*), intent(in) :: char_func_name 

        call testing(char_func_name)
        if (logical_cond) then 
            print '(a)', "Test passed."
        else 
            call error_stop(char_func_name)
        end if 
    end subroutine if_passed

end program test_euler_mpf
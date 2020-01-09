program test_euler_mpf
    use euler_mi_m
    implicit none 

    logical :: cond 
    character(:), allocatable :: func_name
    type(very_long_int_t), pointer :: i1, i2, i3 

    test_assignment: block 
        allocate (i1, i2)
        i1 = '123456'
        i2 = 123456

        func_name = 'assignment'
        cond = i1 == i2 
        
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

    subroutine if_passed(cond, func_name)
        logical, intent(in) :: cond 
        character(len=*), intent(in) :: func_name 

        call testing(func_name)
        if (cond) then 
            print '(a)', "Test passed."
        else 
            call error_stop(func_name)
        end if 
    end subroutine if_passed

end program test_euler_mpf
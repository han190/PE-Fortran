program test_euler_mpf
    use euler_mi_m
    implicit none

    ! Global Variables
    logical :: cond
    character(len=50) :: func_name
    type(very_long_int_t) :: i, j, ans

    test_assignment: block
        integer, allocatable, dimension(:) :: arr

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

    test_addition: block
        i = '20349587294385203457829345728349502934875239485720349587'
        j = '98347529845792834750293847523450923845729834502345723459'
        ans = '118697117140178038208123193251800426780605073988066073046'

        func_name = 'addition'
        cond = i + j == ans

        call if_passed(cond, func_name)
    end block test_addition

    test_subtraction: block
        i = '20349587294385203457829345728349502934875239485720349587'
        j = '98347529845792834750293847523450923845729834502345723459'
        ans = '-77997942551407631292464501795101420910854595016625373872'

        func_name = 'subtraction'
        cond = i - j == ans

        call if_passed(cond, func_name)
    end block test_subtraction

    test_multiplication: block
        i = '20349587294385203457829345728349502934875239485720349587'
        j = '98347529845792834750293847523450923845729834502345723459'
        ans = '2001331643784115457956395522952566745347490594714'// &
              '636927425974011112360503224364938989281368995550671039906861433'

        func_name = 'multiplication'
        cond = i*j == ans

        call if_passed(cond, func_name)
    end block test_multiplication

    contains

    subroutine testing(test_msg)
        character(len=*), intent(in) :: test_msg

        print '(a)', "Testing '"//trim(test_msg)//"' ..."
    end subroutine testing

    subroutine error_stop(err_msg)
        character(len=*), intent(in) :: err_msg

        print '(a)', "Testing '"//trim(err_msg)//"' failed."
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

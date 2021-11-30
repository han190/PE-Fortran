program main

    use utility_m
    implicit none

    call title("'utility_m'")
    block
        integer, parameter :: i = 35124
        integer, parameter :: array(5) = [3, 5, 1, 2, 4]
        logical :: conditions(4)

        conditions(1) = all(to_array(i) == array)
        conditions(2) = to_integer(array) == i
        conditions(3) = unit_digit(i) == 4
        conditions(4) = number_of_digits(i) == 5

        call test(conditions(1), "to_array")
        call test(conditions(2), "to_integer")
        call test(conditions(3), "unit_digit")
        call test(conditions(4), "number_of_digits")

    end block

contains

    subroutine test(input_condition, input_name)
        logical, intent(inout) :: input_condition
        character(*), intent(in) :: input_name

        if (input_condition) then
            print "(a)", "Testing "//input_name//"... passed."
        else
            print "(a)", "Testing "//input_name//"... not passed!"
            stop
        end if
    end subroutine test

    subroutine title(input_name)
        character(*), intent(in) :: input_name

        print "(a)", "Testing "//input_name//"..."
    end subroutine title

end program main

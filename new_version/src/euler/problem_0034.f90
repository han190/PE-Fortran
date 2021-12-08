submodule(interface_m) euler_problem_0034_m
    implicit none

contains

    module character(len=20) function euler0034()
        write (euler0034, "(i20)") answer()
    end function euler0034

    pure integer(i32) function answer()
        integer(i32), parameter :: n = 40585
        integer(i32) :: i

        answer = 0
        do i = 1, n
            if (is_curious(i)) answer = answer + i
        end do
    end function answer

    pure logical function is_curious(n)
        integer(i32), intent(in) :: n
        integer(i32), allocatable :: array(:)
        integer(i32) :: i, temp

        if (any(n == [1, 2])) then
            is_curious = .false.
            return
        end if

        associate (x => to_array(n))
            temp = sum([(factorial(x(i)), i=1, size(x))])
        end associate

        if (temp == n) then
            is_curious = .true.
        else
            is_curious = .false.
        end if
    end function is_curious

    pure integer(i32) function factorial(n)
        integer(i32), intent(in) :: n
        integer(i32) :: i

        factorial = product([(i, i=1, n)])
    end function factorial

end submodule euler_problem_0034_m

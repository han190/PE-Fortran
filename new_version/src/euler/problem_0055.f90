submodule(interface_m) euler_problem_0055_m
    use multiprecision_m
    implicit none

contains

    module character(len=20) function euler0055()
        write (euler0055, "(i20)") answer()
    end function euler0055

    pure integer(i32) function answer()
        integer(i32), parameter :: upper_ = 10000
        integer(i32) :: i

        answer = 0
        do i = 1, upper_
            if (is_lychrel(to_long(i))) answer = answer + 1
        end do
    end function answer

    pure logical function is_lychrel(n)
        type(multiprecision_t), intent(in) :: n
        type(multiprecision_t) :: temp, temp2
        integer(i32) :: i

        i = 0; temp = n
        is_lychrel = .true.

        do while (i <= 50)
            temp2 = reverse(temp) + temp
            if (is_palindromic_(temp2)) then
                is_lychrel = .false.
                return
            end if
            temp = temp2
            i = i + 1
        end do
    end function is_lychrel

    pure logical function is_palindromic_(val)
        type(multiprecision_t), intent(in) :: val

        if (reverse(val) == val) then
            is_palindromic_ = .true.
        else
            is_palindromic_ = .false.
        end if
    end function is_palindromic_

    pure function reverse(val) result(ret)
        type(multiprecision_t), intent(in) :: val
        type(multiprecision_t) :: ret

        allocate (ret%arr(size(val%arr)))
        ret%arr(1:size(val%arr)) = val%arr(size(val%arr):1:-1)
        ret%sgn = val%sgn
    end function reverse

end submodule euler_problem_0055_m

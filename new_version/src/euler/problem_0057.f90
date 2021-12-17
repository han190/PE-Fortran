submodule(interface_m) euler_problem_0057_m
    use big_integer_m
    implicit none

contains

    module character(len=20) function euler0057()
        write (euler0057, "(i20)") answer()
    end function euler0057

    pure integer(i32) function answer()
        integer(i32), parameter :: max_ = 1000
        integer(i32) :: i
        type(big_integer) :: a, b

        i = 1; answer = 0; a = 1; b = 2
        do while (i < max_)
            a = a + b*2
            call swap(a, b)
            a = a + b
            if (size(a%arr) > size(b%arr)) answer = answer + 1
            i = i + 1
        end do
    end function answer

    pure subroutine swap(a, b)
        type(big_integer), intent(inout) :: a, b
        type(big_integer) :: t

        call move_alloc(a%arr, t%arr)
        call move_alloc(b%arr, a%arr)
        call move_alloc(t%arr, b%arr)
    end subroutine swap

end submodule euler_problem_0057_m

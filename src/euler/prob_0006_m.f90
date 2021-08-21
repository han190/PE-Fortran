submodule(euler_interface_m) euler_prob_0006_m
    implicit none

contains

    module character(len=20) function euler0006()
        write (euler0006, "(i20)") answer(100)
    end function euler0006

    pure integer function answer(n)
        integer, intent(in) :: n
        integer :: i, sum_square, square_sum

        sum_square = 0; square_sum = 0
        do i = 1, n
            sum_square = sum_square + i**2
        end do
        square_sum = ((1 + n)*n/2)**2
        answer = square_sum - sum_square
    end function answer

end submodule euler_prob_0006_m

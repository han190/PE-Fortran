submodule(interface_m) euler_problem_0056_m
    use big_integer_m
    implicit none

contains

    module character(len=20) function euler0056()
        write (euler0056, "(i20)") answer()
    end function euler0056

    pure integer(i32) function answer()
        integer(i32), parameter :: const = 89, max_ = 10
        type(big_integer) :: integers(max_)
        integer(i32) :: i, j, sum_(max_, max_)

        integers = [(big_(const + i)**const, i=1, max_)]
        do j = 1, max_
            do i = 1, max_
                integers(i) = big_(const + i)*integers(i)
                sum_(i, j) = sum(integers(i)%arr)
            end do
        end do
        answer = maxval(sum_)
    end function answer

end submodule euler_problem_0056_m

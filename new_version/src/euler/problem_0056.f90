submodule(interface_m) euler_problem_0056_m
    use multiprecision_m
    implicit none

contains

    module character(len=20) function euler0056()
        write (euler0056, "(i20)") answer()
    end function euler0056

    pure integer(i32) function answer()
        integer(i32), parameter :: const = 89, max_ = 10
        type(multiprecision_t) :: integers(max_)
        integer(i32) :: i, j, sum_(max_, max_)

        integers = [(to_long(const + i)**const, i=1, max_)]
        do j = 1, max_
            do i = 1, max_
                integers(i) = to_long(const + i)*integers(i)
                sum_(i, j) = sum(integers(i)%arr)
            end do
        end do
        answer = maxval(sum_)
    end function answer

end submodule euler_problem_0056_m

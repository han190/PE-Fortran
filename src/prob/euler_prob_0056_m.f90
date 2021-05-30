submodule(euler_interface_m) euler_prob_0056_m
    use euler_multiprecision_m
    implicit none

contains

    module character(len=20) function euler0056()
        write (euler0056, "(i20)") answer()
    end function euler0056

    function answer() result(ret)
        integer, parameter :: const = 89, ubound = 10
        type(multiprecision_int_t) :: int_arr(ubound)
        integer :: ret, i, j, sum_arr(ubound, ubound)

        int_arr = [(to_long(const + i)**const, i=1, ubound)]
        do j = 1, ubound
            do i = 1, ubound
                int_arr(i) = to_long(const + i)*int_arr(i)
                sum_arr(i, j) = sum(int_arr(i)%arr)
            end do
        end do
        ret = maxval(sum_arr)
    end function answer

end submodule euler_prob_0056_m

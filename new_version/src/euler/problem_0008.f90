submodule(interface_m) euler_problem_0008_m
    implicit none

contains

    module pure character(len=20) function euler0008()
        write (euler0008, "(i20)") answer()
    end function euler0008

    pure integer(i64) function answer()
        use data_m, only: get_euler_data_0008
        implicit none

        integer(i64) :: array(1000), i
        character(len=:), allocatable :: euler_data(:)

        call get_euler_data_0008(euler_data)
        do i = 1, 20
            associate (sum_ => (i - 1)*50 + 1)
                read (euler_data(i), "(50(i1))") array(sum_:sum_ + 49)
            end associate
        end do

        answer = 0
        do i = 1, 988
            associate (prod => product(array(i:i + 12)))
                if (prod > answer) answer = prod
            end associate
        end do
    end function answer

end submodule euler_problem_0008_m

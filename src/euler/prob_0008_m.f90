submodule(euler_interface_m) euler_prob_0008_m
    implicit none

contains

    module character(len=20) function euler0008()
        write (euler0008, "(i20)") answer()
    end function euler0008

    function answer() result(ret)
        use euler_data_m, only: get_euler_data_0008
        implicit none

        integer(int64) :: big_int(1000), i, s, ret
        character(len=:), allocatable :: euler_data(:)

        call get_euler_data_0008(euler_data)
        do i = 1, 20
            s = (i - 1)*50 + 1
            read (euler_data(i), "(50(i1))") big_int(s:s + 49)
        end do

        ret = 0
        do i = 1, 988
            associate (p => product(big_int(i:i + 12)))
                if (p > ret) ret = p
            end associate
        end do
    end function answer

end submodule euler_prob_0008_m

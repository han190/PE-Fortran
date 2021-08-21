submodule(euler_interface_m) euler_prob_0011_m
    implicit none

contains

    module character(len=20) function euler0011()
        write (euler0011, "(i20)") answer()
    end function euler0011

    integer function answer() result(ret)
        use euler_data_m, only: get_euler_data_0011
        implicit none

        integer :: int_arr(20, 20), box(4, 4)
        integer :: i, j, ret
        character(len=:), allocatable :: euler_data(:)

        call get_euler_data_0011(euler_data)
        read (euler_data, *) int_arr

        ret = 0
        do concurrent (i = 1:17, j = 1:17)
            box = int_arr(i:i + 3, j:j + 3)
            if (max_of(box) > ret) ret = max_of(box)
        end do
    end function answer

    pure integer function max_of(m)
        integer, intent(in) :: m(:, :)
        integer :: d1(4), d2(4)
        integer :: k

        do k = 1, 4
            d1(k) = m(k, k)
            d2(k) = m(k, 5 - k)
        end do

        max_of = max(product(m(1:4, 1)), product(m(1:4, 4)), &
                     product(m(1, 1:4)), product(m(4, 1:4)), &
                     product(d1(1:4)), product(d2(1:4)))
    end function max_of
end submodule euler_prob_0011_m

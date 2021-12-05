submodule(interface_m) euler_problem_0018_m
    implicit none

contains

    module pure character(len=20) function euler0018()
        write (euler0018, "(i20)") answer()
    end function euler0018

    pure integer(i32) function answer()
        use data_m, only: get_euler_data_0018
        implicit none

        integer(i32), parameter :: n = 15
        type(variant_array_t), allocatable :: variant_array(:)
        integer(i32) :: i, j, x
        character(len=:), allocatable :: euler_data(:)

        call get_euler_data_0018(euler_data)
        allocate (variant_array(n))
        do i = 1, n
            allocate (variant_array(i)%array(i))
            read (euler_data(i), *) variant_array(i)%array
        end do

        do j = n - 1, 1, -1
            do i = 1, j
                x = max(variant_array(j + 1)%array(i), &
                        variant_array(j + 1)%array(i + 1))
                variant_array(j)%array(i) = x + variant_array(j)%array(i)
            end do
        end do
        answer = variant_array(1)%array(1)
    end function answer

end submodule euler_problem_0018_m

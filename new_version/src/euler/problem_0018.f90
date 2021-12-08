submodule(interface_m) euler_problem_0018_m
    implicit none

contains

    module character(len=20) function euler0018()
        write (euler0018, "(i20)") answer()
    end function euler0018

    integer(i32) function answer()
        integer(i32), parameter :: n = 15
        type(variant_array_t), allocatable :: variant_array(:)
        integer(i32) :: i, j, x, iunit
        character(len=:), allocatable :: euler_data(:)

        open (newunit=iunit, file="data_0018.txt", &
              status="old", action="read")

        allocate (variant_array(n))
        do i = 1, n
            allocate (variant_array(i)%array(i))
            read (iunit, *) variant_array(i)%array
        end do
        close (iunit)

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

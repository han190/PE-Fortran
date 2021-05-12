submodule(euler_interface_m) euler_prob_0018_m
    implicit none

contains

    module character(len=20) function euler0018()
        write (euler0018, "(i20)") ans(15)
    end function euler0018

    integer function ans(n)
        use euler_var_arr_m, only: var_arr_t
        implicit none
        integer, intent(in) :: n
        type(var_arr_t), allocatable :: var_arr(:)
        integer :: i, j, x, iunit

        allocate (var_arr(n))
        do i = 1, n
            allocate (var_arr(i)%arr(i))
        end do

        iunit = 10018
        open (unit=iunit, file="euler0018.txt", status="old", action="read")
        do i = 1, n
            read (iunit, *) var_arr(i)%arr
        end do
        close (iunit)

        do j = n - 1, 1, -1
            do i = 1, j
                x = max(var_arr(j + 1)%arr(i), var_arr(j + 1)%arr(i + 1))
                var_arr(j)%arr(i) = x + var_arr(j)%arr(i)
            end do
        end do
        ans = var_arr(1)%arr(1)
    end function ans
end submodule euler_prob_0018_m

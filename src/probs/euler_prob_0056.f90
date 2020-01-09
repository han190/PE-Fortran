submodule(euler_interface_m) euler_problem_0056
    use euler_mi_m
    implicit none 

contains

    module character(len=20) function euler0056()
        write (euler0056, "(i20)") ans()
    end function euler0056

    integer function ans()
        type(very_long_int_t), pointer :: i1, i2, i3
        integer :: i, j, maxx

        allocate (i1, i2, i3)

        maxx = 0 
        do i = 90, 99
            do j = 90, 99
                i1 = i; i2 = j
                i3 = i1**i2
                if ( maxx < sum(i3%arr) ) then 
                    maxx = sum(i3%arr)
                end if 
            end do 
        end do 

        ans = maxx
    end function ans

end submodule euler_problem_0056
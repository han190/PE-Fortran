submodule(euler_interface_m) euler_problem_0056
    use euler_mi_m
    implicit none 

contains

    module character(len=20) function euler0056()
        write (euler0056, "(i20)") ans()
    end function euler0056

    integer function ans()
        type(very_long_int_t) :: i1, i2
        integer :: i, j, maxx

        maxx = 0 
        do i = 90, 99
            do j = 90, 99
                i1 = i
                i2 = i1 ** j
                if ( maxx < sum(i2%arr) ) then 
                    maxx = sum(i2%arr)
                end if 
            end do 
        end do 

        ans = maxx
    end function ans

end submodule euler_problem_0056
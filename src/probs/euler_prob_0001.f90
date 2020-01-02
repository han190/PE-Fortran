submodule(euler_interface_m) euler_prob_0001_m
    implicit none

contains 

    module character(len=20) function euler0001()
        write (euler0001, "(i20)") ans(3, 5, 1000)
    end function euler0001 

    integer function ans(i, j, tmp)
        integer, intent(in) :: i, j, tmp 

        ans =                                                                  &
            sum_divisibly_by(i, tmp - 1) +                                     &
            sum_divisibly_by(j, tmp - 1) -                                     &
            sum_divisibly_by(i * j, tmp - 1)
    end function ans 

    integer function sum_divisibly_by(i, j)
        integer, intent(in) :: i, j 
        integer :: tmp 

        tmp = j / i  
        sum_divisibly_by = i * ( tmp * ( tmp + 1 ) ) / 2 
    end function sum_divisibly_by

end submodule euler_prob_0001_m
submodule(euler_interface_m) euler_prob_0013_m 
    implicit none 

contains 

    module character(len=20) function euler0013()
        write (euler0013, "(i20)") ans()
    end function euler0013

    integer(int64) function ans()
        integer(int64) :: long_int(50, 100)
        integer(int64) :: tmp(50), i, j, k, t 
        character(len=500) :: cwd, filename 
        integer :: e 

        call getcwd(cwd); e = len( trim(cwd) )
        filename = cwd(1:e - 3)//"/data/euler0013.txt"
        open(unit = 13, file = filename, status = "old", action = "read")
        do i = 1, 100
            read (13, "(50(i1))") long_int(1_int64:50_int64, i)
        end do
        close (13)

        do j = 50_int64, 1_int64, -1_int64
            tmp(j) = 0_int64
            do i = 1_int64, 100_int64
                tmp(j) = tmp(j) + long_int(j, i)
            end do
        end do

        do j = 50_int64, 2_int64, -1_int64
            t = tmp(j)
            tmp(j) = tmp(j) - ( tmp(j) / 10_int64 ) * 10_int64
            tmp(j - 1_int64) = tmp(j - 1_int64) +                              &
                ( t - tmp(j) ) / 10_int64
        end do

        k = floor( log10( real( tmp(1) ) ) + 1_int64 )
        t = 0_int64
        do i = 1_int64, 10_int64 - k
            t = t + tmp(i) * 10_int64**( 11_int64 - k - i )
        end do
        ans = t 
    end function ans 
end submodule euler_prob_0013_m
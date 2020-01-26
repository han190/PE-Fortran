submodule(euler_interface_m) euler_prob_0008_m
    implicit none 

contains 

    module character(len=20) function euler0008()
        write (euler0008, "(i20)") ans()
    end function euler0008 

    integer(int64) function ans()
        integer(int64) :: long_int(1000), i, s, tmp 
        character(len=500) :: cwd, filename
        integer :: e 

        call getcwd(cwd); e = len( trim(cwd) )
        ! When the binary is being executed, 
        ! the directory is at ../ans, we can 
        ! change it /dat.
        filename = cwd(1:e - 3)//"/dat/euler0008.txt"
        open(unit = 8, file = filename, status = "old", action = "read")
        do i = 1_int64, 20_int64 
            s = ( i - 1_int64 ) * 50_int64 + 1_int64 
            read (8, "(50(i1))") long_int(s:s + 49_int64)
        end do 
        close(8)

        tmp = 0_int64 
        do i = 1_int64, 988_int64 
            if (                                                               &
                product( long_int(i:i + 12_int64) ) > tmp                      &
            ) then 
                tmp = product( long_int(i:i + 12_int64) )
            end if  
        end do 
        ans = tmp
    end function ans 
    
end submodule euler_prob_0008_m
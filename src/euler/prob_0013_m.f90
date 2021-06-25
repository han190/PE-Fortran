submodule(euler_interface_m) euler_prob_0013_m
    use euler_multiprecision_m
    implicit none

contains

    module character(len=20) function euler0013()
        type(multiprecision_int_t) :: val
        val = answer()
        write (euler0013, "(10(' '), 10(i1))") val%arr(1:10)
    end function euler0013

    function answer() result(ret)
        integer :: iunit, i
        type(multiprecision_int_t) :: tmp, ret

        iunit = 10013
        open (unit=iunit, file=data_dir//"euler0013.txt", action="read")
        tmp%sgn = '+'; allocate (tmp%arr(50)); ret = 0
        do i = 1, 100
            read (iunit, "(50(i1))") tmp%arr
            ret = ret + tmp
        end do
        close (iunit)
    end function answer

end submodule euler_prob_0013_m

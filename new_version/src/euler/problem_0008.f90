submodule(interface_m) euler_problem_0008_m
    implicit none

contains

    module character(len=20) function euler0008()
        write (euler0008, "(i20)") answer()
    end function euler0008

    integer(i64) function answer()
        integer(i64) :: array(1000), i, iunit

        open (newunit=iunit, file='data_0008.txt', &
              status='old', action='read')
        do i = 1, 20
            read (iunit, "(50(i1))") array((i - 1)*50 + 1:i*50)
        end do
        close (iunit)

        answer = 0
        do i = 1, 988
            associate (prod => (product(array(i:i + 12))))
                if (prod > answer) answer = prod
            end associate
        end do
    end function answer

end submodule euler_problem_0008_m

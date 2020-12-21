submodule(euler_interface_m) euler_prob_0016_m
    implicit none

contains

    module character(len=20) function euler0016()
        write (euler0016, "(i20)") ans(1000)
    end function euler0016

    integer function ans(n)
        integer, intent(in) :: n
        integer, allocatable :: iarr(:)
        integer :: i, j, l

        l = floor( &
            real(n)* &
            log10(2.0) + 1.0 &
            )

        allocate (iarr(l))
        iarr = 0; iarr(l) = 1

        do i = 1, n
            iarr = iarr*2

            do j = l, 2, -1
                iarr(j - 1) = iarr(j - 1) + iarr(j)/10
                iarr(j) = mod(iarr(j), 10)
            end do
        end do

        ans = sum(iarr, dim=1)
    end function ans

end submodule euler_prob_0016_m

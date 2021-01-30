submodule(euler_interface_m) euler_prob_0020_m
    implicit none

contains

    module character(len=20) function euler0020()
        write (euler0020, "(i20)") ans(100)
    end function euler0020

    integer function ans(n)
        integer, intent(in) :: n
        integer, allocatable :: iarr(:)
        integer :: l, i, j
        real(sp) :: lr

        lr = 0._sp
        do i = 1, n
            lr = lr + log10(real(i, sp))
        end do
        l = floor(lr + 1.)

        allocate (iarr(l))
        iarr = 0
        iarr(l) = 1

        do i = 1, n
            iarr = iarr*i

            do j = l, 2, -1
                iarr(j - 1) = iarr(j - 1) + iarr(j)/10
                iarr(j) = mod(iarr(j), 10)
            end do
        end do
        ans = sum(iarr, dim=1)
    end function ans

end submodule euler_prob_0020_m

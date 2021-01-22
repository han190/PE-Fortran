submodule(euler_interface_m) euler_prob_0011_m
    implicit none

contains

    module character(len=20) function euler0011()
        write (euler0011, "(i20)") ans()
    end function euler0011

    integer function ans()
        integer :: int_arr(20, 20), the_box(4, 4)
        integer :: i, j, prod_max, iunit

        iunit = 10011
        open (unit=iunit, file="euler0011.txt", status="old", action="read")
        read (iunit, *) int_arr
        close (iunit)

        prod_max = 0
        outer: do i = 1, 17
            inner: do j = 1, 17
                the_box = int_arr(i:i + 3, j:j + 3)
                if (max_block(the_box) > prod_max) then
                    prod_max = max_block(the_box)
                end if
            end do inner
        end do outer
        ans = prod_max
    end function ans

    integer function max_block(m)
        integer, intent(in) :: m(:, :)
        integer :: d1(4), d2(4)
        integer :: k

        do k = 1, 4
            d1(k) = m(k, k)
            d2(k) = m(k, 5 - k)
        end do

        max_block = max(product(m(1:4, 1)), product(m(1:4, 4)), &
                        product(m(1, 1:4)), product(m(4, 1:4)), &
                        product(d1(1:4)), product(d2(1:4)))
    end function max_block
end submodule euler_prob_0011_m

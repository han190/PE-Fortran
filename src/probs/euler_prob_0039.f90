submodule(euler_interface_m) euler_prob_0039_m
    implicit none

contains

    module character(len=20) function euler0039()
        write (euler0039, "(i20)") ans(1000)
    end function euler0039

    integer function ans(n)
        integer, intent(in) :: n
        integer :: i, count_arr(n)

        do i = 1, n
            count_arr(i) = right_triangle_with_perimeter(i)
        end do

        ans = maxloc(count_arr, dim=1)
    end function ans

    integer function right_triangle_with_perimeter(p)
        integer, intent(in) :: p
        integer :: i, j, k, t

        t = 0
        do i = 1, p/2 + 1
            do j = 1, p - i - 1
                k = p - i - j
                if (is_right_triangle(i, j, k)) t = t + 1
            end do
        end do
        right_triangle_with_perimeter = t
    end function right_triangle_with_perimeter

    logical function is_right_triangle(i, j, k)
        integer, intent(in) :: i, j, k

        is_right_triangle = .false.
        if (i**2 + j**2 == k**2) then
            is_right_triangle = .true.
        end if
    end function is_right_triangle

end submodule euler_prob_0039_m

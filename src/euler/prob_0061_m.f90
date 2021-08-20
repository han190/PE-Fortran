submodule(euler_interface_m) euler_prob_0061_m
    use euler_var_arr_m, only: var_arr_t
    implicit none

contains

    module character(len=20) function euler0061()
        write (euler0061, "(i20)") answer()
    end function euler0061

    function answer() result(ret)
        integer :: k, ret, idx(6)
        type(var_arr_t) :: polygonals(3:8)
        type(var_arr_t), allocatable :: arr(:)
        logical :: avail

        avail = .true.; ret = 0
        idx = [3, 4, 5, 6, 7, 8]
        call get_polygonals(polygonals)

        do while (avail)
            do k = 1, 6
                call get_cyclic_arr(arr, polygonals(idx(k))%arr)
            end do

            do k = 1, size(arr)
                associate (x => arr(k)%arr, s => size(arr(k)%arr))
                    if (x(1)/100 == mod(x(s), 100) .and. s == 6) then
                        ret = sum(x)
                        return
                    end if
                end associate
            end do
            deallocate (arr)
            call next_permutation(idx, avail)
        end do
    end function answer

    subroutine get_cyclic_arr(arr, arr2)
        type(var_arr_t), allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: arr2(:)
        type(var_arr_t) :: tmp(100)
        integer :: i, j, k

        if (.not. allocated(arr)) then
            allocate (arr(size(arr2)))
            do concurrent (i = 1:size(arr))
                arr(i)%arr = [arr2(i)]
            end do
            return
        end if

        k = 0
        do concurrent (i = 1:100)
            tmp(i)%arr = [0]
        end do

        do concurrent (i = 1:size(arr), j = 1:size(arr2))
            associate (x => arr(i)%arr, y => arr2(j))
                if (mod(x(size(x)), 100) == y/100) then
                    k = k + 1
                    tmp(k)%arr = [x, [y]]
                end if
            end associate
        end do

        if (k /= 0) then
            deallocate (arr)
            allocate (arr(k))
            do concurrent (i = 1:k)
                arr(i)%arr = tmp(i)%arr
            end do
        end if
    end subroutine get_cyclic_arr

    subroutine get_polygonals(polygonals)
        type(var_arr_t), intent(out) :: polygonals(3:8)
        integer :: i, j

        do concurrent (i=3:8, j = 1000:9999)
            if (is_polygonal(i, j)) then
                call append(polygonals(i)%arr, j)
            end if
        end do
    end subroutine get_polygonals

    pure function is_polygonal(n, val) result(ret)
        integer, intent(in) :: n, val
        logical :: ret

        select case (n)
        case (3)
            ret = is_triangle(val)
        case (4)
            ret = is_square(val)
        case (5)
            ret = is_pentagonal(val)
        case (6)
            ret = is_hexaonal(val)
        case (7)
            ret = is_heptagonal(val)
        case (8)
            ret = is_octagonal(val)
        case default
            error stop "Not yet supported"
        end select
    end function is_polygonal

    pure function is_triangle(val) result(ret)
        integer, intent(in) :: val
        logical :: ret

        ret = .false.
        if (is_int(.5*(sqrt(1.+8.*real(val)) - 1.))) ret = .true.
    end function is_triangle

    pure function is_square(val) result(ret)
        integer, intent(in) :: val
        logical :: ret

        ret = .false.
        if (is_int(sqrt(real(val)))) ret = .true.
    end function is_square

    pure function is_pentagonal(val) result(ret)
        integer, intent(in) :: val
        logical :: ret

        ret = .false.
        if (is_int((sqrt(1.+24.*real(val)) + 1.)/6.)) ret = .true.
    end function is_pentagonal

    pure function is_hexaonal(val) result(ret)
        integer, intent(in) :: val
        logical :: ret

        ret = .false.
        if (is_int(.25*(sqrt(1.+8.*real(val)) + 1.))) ret = .true.
    end function is_hexaonal

    pure function is_heptagonal(val) result(ret)
        integer, intent(in) :: val
        logical :: ret

        ret = .false.
        if (is_int((sqrt(9.+40.*real(val)) + 3.)/10.)) ret = .true.
    end function is_heptagonal

    pure function is_octagonal(val) result(ret)
        integer, intent(in) :: val
        logical :: ret

        ret = .false.
        if (is_int((sqrt(1.+3.*real(val)) + 1.)/3.)) ret = .true.
    end function is_octagonal

    pure function is_int(val) result(ret)
        real, intent(in) :: val
        logical :: ret
        real, parameter :: eps = tiny(0.)

        ret = .false.
        if (abs(val - nint(val)) < eps) ret = .true.
    end function is_int

end submodule euler_prob_0061_m

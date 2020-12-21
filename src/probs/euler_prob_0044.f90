submodule(euler_interface_m) euler_prob_0044_m
    implicit none
    integer(int64), parameter :: dim_of_arr = 3000_int64
    integer(int64) :: pen_arr(dim_of_arr)

contains

    module character(len=20) function euler0044()
        write (euler0044, "(i20)") ans()
    end function euler0044

    integer(int64) function ans()
        integer(int64) :: i, j, minimised_d
        integer(int64) :: pm, pn

        j = 1_int64
        i = 1_int64

        do while (i <= dim_of_arr)
            if (is_pen(j)) then
                pen_arr(i) = j
                i = i + 1_int64
            end if
            j = j + 1_int64
        end do

        ! set minimised_d to a big number
        minimised_d = huge(0_int64)

        do i = 1_int64, dim_of_arr
            do j = i + 1_int64, dim_of_arr
                pm = pen_arr(i)
                pn = pen_arr(j)
                if (is_pen_sum_diff(pm, pn)) then
                    minimised_d = min(minimised_d, pn - pm)
                end if
            end do
        end do

        ans = minimised_d
    end function ans

    logical function is_pen_sum_diff(pj, pk)
        integer(int64), intent(in) :: pj, pk
        integer(int64) :: s, d

        if (pj > pk) then
            error stop "is_pen_SUM_DIFF: pj < pk"
        end if

        s = pj + pk
        d = pk - pj

        if (is_pen(s) .and. is_pen(d)) then
            is_pen_sum_diff = .true.
        else
            is_pen_sum_diff = .false.
        end if
    end function is_pen_sum_diff

    logical function is_pen(p)
        integer(int64), intent(in) :: p

        if (p <= 0) then
            error stop "is_pen: p > 0."
        end if

        associate (x => sqrt(24.0_sp*real(p) + 1.0_sp))
            if ( &
                is_int(x) .and. &
                mod(int(x, int64), 6_int64) == 5_int64 &
                ) then
                is_pen = .true.
            else
                is_pen = .false.
            end if
        end associate
    end function is_pen

    logical function is_int(n)
        real, intent(in) :: n

        is_int = .false.
        if (n - floor(n) <= tiny_sp) then
            is_int = .true.
        end if
    end function is_int

end submodule euler_prob_0044_m

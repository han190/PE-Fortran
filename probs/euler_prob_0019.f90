submodule(euler_interface_m) euler_prob_0019_m
    implicit none

contains

    module character(len=20) function euler0019()
        write (euler0019, "(i20)") ans(1901, 2000)
    end function euler0019

    integer function ans(from, to)
        integer, intent(in) :: from, to
        integer :: month(12), dow
        integer :: tmp, i, j

        month = [ &
                31, 28, 31, 30, &
                31, 30, 31, 31, &
                30, 31, 30, 31 &
                ]

        tmp = 0; dow = 1
        outer: do i = from, to
            if (is_leap(i)) then
                month(2) = 29
            end if

            inner: do j = 1, size(month)
                dow = dow + mod(month(j), 7) - 1
                if (mod(dow, 7) == 0) then
                    tmp = tmp + 1
                end if
            end do inner
        end do outer
        ans = tmp
    end function ans

    logical function is_leap(n)
        integer, intent(in) :: n

        is_leap = .false.
        if ( &
            ( &
            (mod(n, 4) == 0) .and. &
            (mod(n, 100) /= 0) &
            ) .or. &
            (mod(n, 400) == 0) &
            ) then
            is_leap = .true.
        end if
    end function is_leap

end submodule euler_prob_0019_m

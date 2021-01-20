submodule(euler_interface_m) euler_prob_0023_m
    implicit none
    ! For array operations that might create array temporary
    ! I comment the code and used do loop instead for purpose of performance.

contains

    module character(len=20) function euler0023()
        write (euler0023, "(i20)") ans(28123)
    end function euler0023

    integer function ans(n)
        integer, intent(in) :: n
        integer, dimension(n) :: sums_of_divs, bndnt_drft, nums
        integer, allocatable :: bndnt_arr(:)
        integer :: i, j, tmp
        logical :: can_be_written(n)

        do i = 1, n
            do j = i*2, n, i
                sums_of_divs(j) = sums_of_divs(j) + i
            end do
        end do

        bndnt_drft = 0
        do i = 1, n
            if (sums_of_divs(i) > i) then
                bndnt_drft(i) = i
            end if
        end do

        allocate (bndnt_arr(count(bndnt_drft /= 0)))

        ! bndnt_arr = pack(                                                    &
        !     bndnt_drft,                                                      &
        !     bndnt_drft /= 0                                                  &
        ! )
        j = 1
        do i = 1, size(bndnt_drft)
            if (bndnt_drft(i) /= 0) then
                bndnt_arr(j) = bndnt_drft(i)
                j = j + 1
            end if
        end do

        can_be_written = .true.
        outer: do i = 1, size(bndnt_arr)
            inner: do j = 1, size(bndnt_arr)
                associate (x => bndnt_arr(i) + bndnt_arr(j))
                    if (x < n + 1) then
                        can_be_written(x) = .false.
                    else
                        exit inner
                    end if
                end associate
            end do inner
        end do outer

        nums = [(i, i=1, n)]
        tmp = sum(pack(nums, can_be_written), dim=1)
        ans = tmp
    end function ans
end submodule euler_prob_0023_m

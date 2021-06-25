submodule(euler_interface_m) euler_prob_0054_m
    implicit none

contains

    module character(len=20) function euler0054()
        write (euler0054, "(i20)") ans()
    end function euler0054

    integer function ans()
        character(len=2) :: str_arr(10)
        integer :: istat, iunit, i

        iunit = 10054
        open (unit=iunit, file=data_dir//"euler0054.txt", action="read")
        i = 0
        do
            read (iunit, *, iostat=istat) str_arr
            if (istat /= 0) exit
            if (player_one_win(str_arr)) i = i + 1
        end do
        close (iunit)
        ans = i
    end function ans

    subroutine to_arrs(hands, vals, suits)
        character(len=2), intent(in) :: hands(5)
        integer, intent(out) :: vals(1:14), suits(1:4)
        character(len=1) :: value_arr(1:14), suit_arr(1:4)
        integer :: i, v, s

        value_arr = ['A', '2', '3', '4', '5', '6', '7', &
                     '8', '9', 'T', 'J', 'Q', 'K', 'A']
        suit_arr = ['S', 'H', 'C', 'D']
        vals = 0; suits = 0

        do i = 1, 5
            v = findloc(value_arr(:), hands(i) (1:1), dim=1, back=.true.)
            s = findloc(suit_arr(:), hands(i) (2:2), dim=1)
            vals(v) = vals(v) + 1
            if (v == 14) vals(1) = vals(1) + 1
            suits(s) = suits(s) + 1
        end do
    end subroutine to_arrs

    subroutine rank(hands, s_arr)
        character(len=2), intent(in) :: hands(5)
        integer, intent(out) :: s_arr(6)
        integer :: vals(1:14), suits(1:4), x, y, z

        call to_arrs(hands, vals, suits)
        s_arr = 0

        ! Royal Flush
        if (all(vals(10:14) == 1) .and. any(suits(:) == 5)) then
            x = findloc(suits, 5, dim=1)
            s_arr(1:2) = [10, x]
            return
        end if

        ! Straight flush
        if (any(suits(:) == 5)) then
            do x = 1, 10
                if (all(vals(x:x + 4) == 1)) then
                    y = findloc(suits, 5, dim=1)
                    s_arr(1:3) = [9, x, y]
                    return
                end if
            end do
        end if

        ! Four of a kind
        if (any(vals(:) == 4)) then
            x = findloc(vals, 4, dim=1, back=.true.)
            y = findloc(vals, 1, dim=1, back=.true.)
            s_arr(1:3) = [8, x, y]
            return
        end if

        ! Full house
        if (any(vals(:) == 3) .and. any(vals(:) == 2)) then
            x = findloc(vals, 3, dim=1, back=.true.)
            y = findloc(vals, 2, dim=1, back=.true.)
            s_arr(1:3) = [7, x, y]
            return
        end if

        ! Flush
        if (any(suits(:) == 5)) then
            s_arr(1) = 6
            call knt_one_by_one(2, vals, s_arr)
            return
        end if

        ! Straight
        do x = 1, 10
            if (all(vals(x:x + 4) == 1)) then
                s_arr(1:2) = [5, x]
                return
            end if
        end do

        ! Three of a kind
        if (any(vals(:) == 3)) then
            x = findloc(vals, 3, dim=1, back=.true.)
            y = findloc(vals, 1, dim=1, back=.true.)
            z = findloc(vals(2:14), 1, dim=1) + 1
            s_arr(1:4) = [4, x, y, z]
            return
        end if

        ! Two pairs
        if (count(vals(2:14) == 2) == 2) then
            x = findloc(vals, 2, dim=1, back=.true.)
            y = findloc(vals(2:14), 2, dim=1) + 1
            z = findloc(vals, 1, dim=1, back=.true.)
            s_arr(1:4) = [3, x, y, z]
            return
        end if

        ! One pair
        if (any(vals(:) == 2)) then
            x = findloc(vals, 2, dim=1, back=.true.)
            s_arr(1:2) = [2, x]
            call knt_one_by_one(3, vals, s_arr)
            return
        end if

        ! High card
        s_arr(1) = 1
        call knt_one_by_one(2, vals, s_arr)

    contains

        subroutine knt_one_by_one(x_, val_arr, output_arr)
            integer, intent(in) :: val_arr(:), x_
            integer, intent(out) :: output_arr(:)
            integer :: idx, tmp

            tmp = x_
            do idx = 14, 2, -1
                if (tmp >= 7) exit
                if (val_arr(idx) == 1) then
                    output_arr(tmp) = idx
                    tmp = tmp + 1
                end if
            end do
        end subroutine knt_one_by_one

    end subroutine rank

    function player_one_win(str_arr) result(ret)
        character(len=2), intent(in) :: str_arr(10)
        logical :: ret
        integer :: rank1(6), rank2(6), i

        call rank(str_arr(1:5), rank1)
        call rank(str_arr(6:10), rank2)

        do i = 1, 6
            if (rank1(i) > rank2(i)) then
                ret = .true.
                return
            else if (rank1(i) < rank2(i)) then
                ret = .false.
                return
            end if
        end do

        error stop "There is no tie."
    end function player_one_win

end submodule euler_prob_0054_m

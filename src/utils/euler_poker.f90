module euler_poker_m
    implicit none
    private

    type, public :: poker_t
        character(len=2) :: hands(5)
        contains
        procedure, private :: initialize_sub
        generic :: assignment(=) => initialize_sub
        procedure :: to_arrs => to_arrs_sub
        procedure :: rank => rank_sub
    end type poker_t

contains

    subroutine initialize_sub(this, chr)
        class(poker_t), intent(inout) :: this
        character(len=2), intent(in) :: chr(5)
        integer :: i

        do i = 1, 5
            this%hands(i) (1:2) = chr(i) (1:2)
        end do
    end subroutine initialize_sub

    subroutine to_arrs_sub(this, vals, suits)
        class(poker_t) :: this
        integer, intent(out) :: vals(1:14), suits(1:4)
        character(len=1) :: value_arr(1:14), suit_arr(1:4)
        integer :: i, v, s

        value_arr = ['A', '2', '3', '4', '5', '6', '7', &
                     '8', '9', 'T', 'J', 'Q', 'K', 'A']
        suit_arr = ['S', 'H', 'C', 'D']
        vals = 0; suits = 0

        do i = 1, 5
            v = findloc(value_arr(:), this%hands(i) (1:1), &
                        dim=1, back=.true.)

            s = findloc(suit_arr(:), this%hands(i) (2:2), dim=1)

            vals(v) = vals(v) + 1
            if (v == 14) then
                vals(1) = vals(1) + 1
            end if

            suits(s) = suits(s) + 1
        end do
    end subroutine to_arrs_sub

    subroutine rank_sub(this, s_arr)
        class(poker_t) :: this
        integer, intent(out) :: s_arr(6)
        integer :: vals(1:14), suits(1:4), x, y, z

        call this%to_arrs(vals, suits)
        s_arr = 0

        ! Royal Flush
        if (all(vals(10:14) == 1) .and. any(suits(:) == 5)) then
            x = findloc(suits(:), 5, dim=1)
            s_arr(1:2) = [10, x]
            return
        end if

        ! Straight flush
        if (any(suits(:) == 5)) then
            do x = 1, 10
                if (all(vals(x:x + 4) == 1)) then
                    y = findloc(suits(:), 5, dim=1)
                    s_arr(1:3) = [9, x, y]
                    return
                end if
            end do
        end if

        ! Four of a kind
        if (any(vals(:) == 4)) then
            x = findloc(vals(:), 4, dim=1, back=.true.)
            y = findloc(vals(:), 1, dim=1, back=.true.)
            s_arr(1:3) = [8, x, y]
            return
        end if

        ! Full house
        if (any(vals(:) == 3) .and. any(vals(:) == 2)) then
            x = findloc(vals(:), 3, dim=1, back=.true.)
            y = findloc(vals(:), 2, dim=1, back=.true.)
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
            x = findloc(vals(:), 3, dim=1, back=.true.)
            y = findloc(vals(:), 1, dim=1, back=.true.)
            z = findloc(vals(2:14), 1, dim=1) + 1
            s_arr(1:4) = [4, x, y, z]
            return
        end if

        ! Two pairs
        if (count(vals(2:14) == 2) == 2) then
            x = findloc(vals(:), 2, dim=1, back=.true.)
            y = findloc(vals(2:14), 2, dim=1) + 1
            z = findloc(vals(:), 1, dim=1, back=.true.)
            s_arr(1:4) = [3, x, y, z]
            return
        end if

        ! One pair
        if (any(vals(:) == 2)) then
            x = findloc(vals(:), 2, dim=1, back=.true.)
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

    end subroutine rank_sub
    
end module euler_poker_m

module euler_texas_holdem_m
    use euler_poker_m
    implicit none
    private

    type, public :: texas_holdem_t
        type(poker_t) :: decks(2)
        contains
        procedure, private :: initialize_sub
        generic :: assignment(=) => initialize_sub
        procedure, private :: compare_sub
        generic :: operator(.playerOneWin.) => compare_sub
    end type texas_holdem_t

contains

    subroutine initialize_sub(this, chr)
        class(texas_holdem_t), intent(inout) :: this
        character(len=2), intent(in) :: chr(10)

        this%decks(1) = chr(1:5)
        this%decks(2) = chr(6:10)
    end subroutine initialize_sub

    function compare_sub(this) result(ans)
        class(texas_holdem_t), intent(in) :: this
        integer :: arr1(6), arr2(6), i
        logical :: ans

        call this%decks(1)%rank(arr1)
        call this%decks(2)%rank(arr2)

        do i = 1, 6
            if (arr1(i) > arr2(i)) then
                ans = .true.
                return
            else if (arr1(i) < arr2(i)) then
                ans = .false.
                return
            end if
        end do

        error stop "There is no tie."
    end function compare_sub

end module euler_texas_holdem_m

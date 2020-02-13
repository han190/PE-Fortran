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

    character(len=1), dimension(1:14), parameter ::                            &
        value_arr = [                                                          &
            'A', '2', '3', '4', '5',                                           &
            '6', '7', '8', '9',                                                &
            'T', 'J', 'Q', 'K', 'A'                                            &
        ]

    character(len=1), dimension(1:4), parameter ::                             &
        suit_arr = [                                                           &
            'S', 'H', 'C', 'D'                                                 &
        ]

contains

    subroutine initialize_sub(this, chr)
        class(poker_t), intent(inout) :: this 
        character(len=2), dimension(5), intent(in) :: chr
        integer :: i

        do i = 1, 5
            this%hands(i)(1:2) = chr(i)(1:2)
        end do 
    end subroutine initialize_sub

    subroutine to_arrs_sub(this, vals, suits)
        class(poker_t) :: this 
        integer, dimension(1:14), intent(out) :: vals
        integer, dimension(1:4), intent(out) :: suits
        integer :: i, v, s

        vals = 0; suits = 0

        do i = 1, 5
            v = findloc(                                                       &
                value_arr(:), this%hands(i)(1:1),                              &
                dim = 1, back = .true.                                         &
            )

            s = findloc(                                                       &
                suit_arr(:), this%hands(i)(2:2),                               &
                dim = 1                                                        &
            )

            vals(v) = vals(v) + 1
            if (v == 14) then 
                vals(1) = vals(1) + 1
            end if 

            suits(s) = suits(s) + 1
        end do 
    end subroutine to_arrs_sub

    subroutine rank_sub(this, s_arr)
        class(poker_t) :: this 
        integer, dimension(6), intent(out) :: s_arr
        integer, dimension(1:14) :: vals
        integer, dimension(1:4) :: suits
        integer :: i, x, y

        call to_arrs_sub(this, vals, suits)

        ! Royal Flush
        if (                                                                   &
            all( vals(10:14) == 1 ) .and.                                      &
            any( suits(:) == 5 )                                               &
        ) then 
            s_arr(1) = 10
            s_arr(2:) = 0
            return 
        end if 

        ! Straight flush
        if ( any( suits(:) == 5 ) ) then 
            do i = 1, 10
                if ( all( vals(i:i + 4) == 1 ) ) then 
                    s_arr(1) = 9
                    s_arr(2) = i
                    s_arr(3:) = 0
                    return 
                end if 
            end do
        end if 

        ! Four of a kind 
        if ( any( vals(:) == 4 ) ) then
            x = findloc( vals(:), 4, dim = 1, back = .true. )
            y = findloc( vals(:), 1, dim = 1, back = .true. )

            s_arr(1) = 8
            s_arr(2) = x
            s_arr(3) = y 
            s_arr(4:) = 0
            return 
        end if 

        ! Full house
        if (                                                                   &
            any( vals(:) == 3 ) .and.                                          &
            any( vals(:) == 2 )                                                &
        ) then 
            x = findloc( vals(:), 3, dim = 1, back = .true. )
            y = findloc( vals(:), 2, dim = 1, back = .true. )

            s_arr(1) = 7 
            s_arr(2) = x 
            s_arr(3) = y 
            s_arr(4:) = 0
            return 
        end if 

        ! Flush
        if ( any( suits(:) == 5 ) ) then
            s_arr(:) = 0 
            s_arr(1) = 6
            x = 2

            do i = 14, 2, -1
                if (x >= 7) exit 

                if ( vals(i) /= 0 ) then 
                    s_arr(x) = i
                    x = x + 1
                end if 
            end do 

            return 
        end if 

        ! Straight
        do i = 1, 10
            if ( all( vals(i:i + 4) == 1 ) ) then
                s_arr(1) = 5 
                s_arr(2) = i
                s_arr(3:) = 0
                return 
            end if 
        end do

        ! Three of a kind 
        if ( any( vals(:) == 3 ) ) then 
            x = findloc( vals(:), 3, dim = 1, back = .true. )
            s_arr(1) = 4
            s_arr(2) = x

            x = findloc( vals(:), 1, dim = 1, back = .true. )
            y = findloc( vals(2:14), 1, dim = 1 ) + 1
            s_arr(3) = x
            s_arr(4) = y
            s_arr(5:6) = 0
            return 
        end if

        ! Two pairs
        if ( count( vals(2:14) == 2 ) == 2 ) then 
            x = findloc( vals(:), 2, dim = 1, back = .true. )
            y = findloc( vals(2:14), 2, dim = 1 ) + 1
            s_arr(1) = 3
            s_arr(2) = x
            s_arr(3) = y

            x = findloc( vals(:), 1, dim = 1, back = .true.)
            s_arr(4) = x
            s_arr(5:) = 0
            return 
        end if 

        ! One pair
        if ( any( vals(:) == 2 ) ) then 
            x = findloc( vals(:), 2, dim = 1, back = .true. )
            s_arr(:) = 0 
            s_arr(1) = 2
            s_arr(2) = x

            x = 3

            do i = 14, 2, -1
                if (x >= 7) exit 

                if ( vals(i) == 1 ) then 
                    s_arr(x) = i
                    x = x + 1
                end if 
            end do 

            return 
        end if 

        ! High card
        s_arr(1) = 1
        x = 2

        do i = 14, 2, -1
            if (x >= 7) exit 

            if ( vals(i) == 1 ) then 
                s_arr(x) = i
                x = x + 1
            end if 
        end do 
    end subroutine 

end module euler_poker_m

module euler_texas_holdem_m
    use euler_poker_m
    implicit none
    private

    type, public :: texas_holdem_t
        type(poker_t), dimension(2) :: decks
    contains
        procedure, private :: initialize_sub
        generic :: assignment(=) => initialize_sub
        procedure :: compare => compare_sub
    end type texas_holdem_t

contains 

    subroutine initialize_sub(this, chr)
        class(texas_holdem_t), intent(inout) :: this 
        character(len=2), dimension(10), intent(in) :: chr

        this%decks(1) = chr(1:5)
        this%decks(2) = chr(6:10)
    end subroutine initialize_sub

    function compare_sub(this) result(ans)
        class(texas_holdem_t), intent(in) :: this
        integer, dimension(6) :: arr1, arr2
        logical :: ans 
        integer :: i

        call this%decks(1)%rank(arr1)
        call this%decks(2)%rank(arr2)

        do i = 1, 6
            if ( arr1(i) > arr2(i) ) then 
                ans = .true.
                return 
            else if ( arr1(i) < arr2(i) ) then 
                ans = .false.
                return 
            end if 
        end do 

        error stop "There is no tie."
    end function compare_sub

end module euler_texas_holdem_m
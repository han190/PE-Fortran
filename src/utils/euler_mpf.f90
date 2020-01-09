module euler_mi_m
    implicit none 
    private

    type, public :: very_long_int_t
        integer, allocatable :: arr(:)
        character(1) :: sign 
    contains 
        procedure, private :: init_char_sub, init_int_sub, init_arr_sub
        generic :: assignment(=) => init_char_sub, init_int_sub, init_arr_sub
        procedure, private :: equal_func, equal_int_func, equal_char_func
        generic :: operator(==) => equal_func, equal_int_func, equal_char_func
        procedure, private :: greater_than_func
        generic :: operator(>) => greater_than_func
        procedure, private :: less_than_func
        generic :: operator(<) => less_than_func
        procedure, private :: add_func
        generic :: operator(+) => add_func 
        procedure, private :: subtract_func
        generic :: operator(-) => subtract_func
        procedure, private :: multiply_func
        generic :: operator(*) => multiply_func
        procedure, private :: pow_func
        generic :: operator(**) => pow_func
    end type very_long_int_t 

    type, private :: mtrx_t
        integer, allocatable :: iarr(:)
    end type mtrx_t

contains

    subroutine re_alloc(arr, n)
        integer, allocatable, intent(inout) :: arr(:)
        integer, intent(in) :: n

        if ( allocated(arr) ) then 
            deallocate(arr)
        end if 
        allocate( arr(n) )
    end subroutine re_alloc

    subroutine init_char_sub(this, char)
        class(very_long_int_t), intent(inout) :: this
        character(len=*), intent(in) :: char
        integer :: i

        select case ( char(1:1) )
        case('+')
            this%sign = '+'
            call re_alloc( this%arr, len(char) - 1 )
            do i = 2, len(char) 
                read ( char(i:i), * ) this%arr(i - 1)
            end do 
        case('-')
            this%sign = '-'
            call re_alloc( this%arr, len(char) - 1 )
            do i = 2, len(char) 
                read ( char(i:i), * ) this%arr(i - 1)
            end do 
        case default 
            this%sign = '+'
            call re_alloc( this%arr, len(char) )
            do i = 1, len(char) 
                read ( char(i:i), * ) this%arr(i)
            end do 
        end select
    end subroutine init_char_sub

    subroutine init_arr_sub(this, arr)
        class(very_long_int_t), intent(inout) :: this
        integer, allocatable, intent(in) :: arr(:)
        
        call re_alloc( this%arr, size(arr) )
        this%arr = arr
        this%sign = '+'
    end subroutine init_arr_sub

    subroutine init_int_sub(this, int)
        class(very_long_int_t), intent(inout) :: this 
        integer, intent(in) :: int 
        integer :: tmp, i, l 

        if (int >= 0) then 
            this%sign = '+'
        else if (int < 0) then 
            this%sign = '-'
        end if 

        tmp = abs(int)
        l = floor( log10( real(tmp) ) ) + 1
        call re_alloc(this%arr, l)

        do i = l, 1, -1
            this%arr(i) = mod(tmp, 10)
            tmp = tmp / 10
        end do 
    end subroutine init_int_sub

    logical function equal_func(a, b)
        class(very_long_int_t), intent(in) :: a 
        type(very_long_int_t), intent(in) :: b 

        if ( all(a%arr == b%arr) ) then 
            equal_func = .true.
        else 
            equal_func = .false. 
        end if 
    end function equal_func

    logical function equal_int_func(a, b)
        class(very_long_int_t), intent(in) :: a 
        integer, intent(in) :: b 
        type(very_long_int_t) :: tmp 

        tmp = b 
        if (a == tmp) then
            equal_int_func = .true.
        else 
            equal_int_func = .false.
        end if 
    end function equal_int_func

    logical function equal_char_func(a, b)
        class(very_long_int_t), intent(in) :: a 
        character(len=*), intent(in) :: b 
        integer :: i, tmp

        equal_char_func = .true.

        if ( size(a%arr) /= len(b) ) then 
            equal_char_func = .false.
            return 
        end if 

        check_loop: do i = 1, len(b)
            read ( b(i:i), * ) tmp

            if ( a%arr(i) == tmp ) then
                cycle check_loop
            else
                equal_char_func = .false.
                return 
            end if 
        end do check_loop
    end function equal_char_func

    logical function greater_than_func(a, b)
        class(very_long_int_t), intent(in) :: a 
        type(very_long_int_t), intent(in) :: b
        integer :: i

        if (a == b) then
            greater_than_func = .false.; return
        end if 

        if (a%sign == '+' .and. b%sign == '-') then 
            greater_than_func = .true.; return 
        else if (a%sign == '-' .and. b%sign == '+') then 
            greater_than_func = .false.; return 
        else if (a%sign == '+' .and. b%sign == '+') then 

            if ( size(a%arr) > size(b%arr) ) then 
                greater_than_func = .true.; return
            else if ( size(a%arr) < size(b%arr) ) then 
                greater_than_func = .false.; return
            else
                greater_than_func = .true.

                do i = 1, size(a%arr)
                    if ( a%arr(i) < b%arr(i) ) then 
                        greater_than_func = .false.; return 
                    end if 
                end do 
            end if 

        else if (a%sign == '-' .and. b%sign == '-') then 

            if ( size(a%arr) < size(b%arr) ) then 
                greater_than_func = .true.; return
            else if ( size(a%arr) > size(b%arr) ) then 
                greater_than_func = .false.; return
            else
                greater_than_func = .true.

                do i = 1, size(a%arr)
                    if ( a%arr(i) > b%arr(i) ) then 
                        greater_than_func = .false.; return 
                    end if 
                end do 
            end if 

        end if 
    end function greater_than_func

    logical function less_than_func(a, b)
        class(very_long_int_t), intent(in) :: a 
        type(very_long_int_t), intent(in) :: b

        less_than_func = .true.
        if (a == b .or. a > b) then 
            less_than_func = .false.
        end if 
    end function less_than_func

    subroutine cut_leading_zeros(arr)
        integer, allocatable, intent(inout) :: arr(:)
        integer, allocatable :: tmp(:)
        integer :: i

        i = 1 
        do 
            if ( arr(i) /= 0 ) exit 
            i = i + 1
        end do 

        allocate ( tmp( i:size(arr) ) )
        tmp = arr(i:)
        call move_alloc(tmp, arr)
    end subroutine cut_leading_zeros

    subroutine carry_sub(arr)
        integer, allocatable, intent(inout) :: arr(:)
        integer, allocatable :: tmp1(:), tmp2(:)

        allocate( &
            tmp1( size(arr) + 2 ), &
            tmp2( size(arr) + 2 ) &
        )

        tmp1(1:2) = 0
        tmp1(3:) = arr - arr / 10 * 10
        tmp2(1) = 0
        tmp2( size(tmp2) ) = 0 
        tmp2( 2:size(tmp2) - 1 ) = arr / 10
        arr = tmp1 + tmp2 

        do
            if ( all(arr <= 9) ) exit 
            tmp1 = arr - arr / 10 * 10
            tmp2( 1:size(tmp2) - 1 ) = arr(2:) / 10
            arr = tmp1 + tmp2
        end do 
    end subroutine carry_sub

    function core_add_func(arr1, arr2) result(ans)
        integer, allocatable, intent(in) :: arr1(:), arr2(:)
        integer, allocatable :: ans(:)
        integer, allocatable :: tmp1(:), tmp2(:), tmp3(:)

        associate( &
            x => max( size(arr1), size(arr2) ) + 1 &
        )
            allocate( tmp1(x), tmp2(x), tmp3(x) )
            tmp1 = 0; tmp2 = 0; tmp3 = 0
            tmp1( x - size(arr1) + 1:x ) = arr1 
            tmp2( x - size(arr2) + 1:x ) = arr2 
        end associate

        tmp3 = tmp1 + tmp2 
        call carry_sub(tmp3)
        call cut_leading_zeros(tmp3)
        ans = tmp3 
    end function core_add_func

    function core_subtract_func(arr1, arr2) result(ans)
        integer, allocatable, intent(in) :: arr1(:), arr2(:)
        integer, allocatable :: ans(:)
        integer, allocatable :: tmp1(:), tmp2(:), tmp3(:)
        integer :: i

        associate( &
            x => max( size(arr1), size(arr2) ) + 1 &
        )
            allocate( tmp1(x), tmp2(x), tmp3(x) )
            tmp1 = 0; tmp2 = 0; tmp3 = 0
            tmp1( x - size(arr1) + 1:x ) = arr1 
            tmp2( x - size(arr2) + 1:x ) = arr2 
        end associate

        do i = size(tmp1), 2, -1
            if (tmp1(i) >= tmp2(i)) then 
                tmp3(i) = tmp1(i) - tmp2(i)
            else 
                tmp1(i) = tmp1(i) + 10
                tmp1(i - 1) = tmp1(i - 1) - 1
                tmp3(i) = tmp1(i) - tmp2(i)
            end if 
        end do 

        call cut_leading_zeros(tmp3)
        ans = tmp3 
    end function core_subtract_func

    function core_multiply_func(arr1, arr2) result(ans)
        integer, allocatable, intent(in) :: arr1(:), arr2(:)
        integer, allocatable :: ans(:)
        type(mtrx_t), allocatable :: mtrx(:)
        integer, allocatable :: tmp(:)
        integer :: i

        associate(                                                             &
            x => size(arr1) + size(arr2) ,                                     &
            y => size(arr2)                                                    &
        )
            allocate( mtrx(y) )
            
            do i = 1, y
                allocate( mtrx(i)%iarr(x) )
                tmp = arr1 * arr2( size(arr2) - i + 1 )
                call carry_sub(tmp)
                call cut_leading_zeros(tmp)
                associate( row => mtrx(i)%iarr )
                    row = 0
                    row(                                                       &
                        size(row) - size(tmp) - i + 2:                         &
                        size(row) - i + 1                                      &
                    ) = tmp
                end associate
            end do 

            call re_alloc(tmp, size(mtrx(1)%iarr) )
            tmp = 0 ! Initialize array tmp
            do i = 1, y 
                tmp = tmp + mtrx(i)%iarr
            end do 

            call carry_sub(tmp)
            call cut_leading_zeros(tmp)
            ans = tmp   
        end associate 
    end function core_multiply_func

    function add_func(a, b) result(ans)
        class(very_long_int_t), intent(in) :: a
        type(very_long_int_t), intent(in) :: b
        type(very_long_int_t) :: ans

        if (a%sign == '+' .and. b%sign == '+') then 
            ans = core_add_func(a%arr, b%arr)
        else if (a%sign == '-' .and. b%sign == '-') then 
            ans%arr = core_add_func(a%arr, b%arr)
            ans%sign = '-'
        else if (a%sign == '+' .and. b%sign == '-') then 
            ans = core_subtract_func(b%arr, a%arr)
        else if (a%sign == '-' .and. b%sign == '+') then 
            ans%arr = core_subtract_func(b%arr, a%arr)
            ans%sign = '-'
        end if 
    end function add_func 

    function subtract_func(a, b) result(ans)
        class(very_long_int_t), intent(in) :: a 
        type(very_long_int_t), intent(in) :: b 
        type(very_long_int_t) :: ans 

        if (a%sign == '+' .and. b%sign == '+') then 
            if (a > b) then
                ans = core_subtract_func(a%arr, b%arr)
            else if (a == b) then
                ans = '0'
            else
                ans%arr = core_subtract_func(b%arr, a%arr)
                ans%sign = '-'
            end if 
        else if (a%sign == '+' .and. b%sign == '-') then 
            ans%arr = core_add_func(a%arr, b%arr)
            ans%sign = '+'
        else if (a%sign == '-' .and. b%sign == '+') then 
            ans%arr = core_add_func(a%arr, b%arr)
            ans%sign = '-'
        else if (a%sign == '-' .and. b%sign == '-') then 
            if (a > b) then
                ans%arr = core_subtract_func(b%arr, a%arr)
                ans%sign = '+'
            else if (a == b) then
                ans = '0'
            else
                ans%arr = core_subtract_func(a%arr, b%arr)
                ans%sign = '-'
            end if 
        end if 
    end function subtract_func

    function multiply_func(a, b) result(ans)
        class(very_long_int_t), intent(in) :: a 
        type(very_long_int_t), intent(in) :: b 
        type(very_long_int_t) :: ans 

        ans = core_multiply_func(a%arr, b%arr)
    end function multiply_func

    function pow_func(a, b) result(ans)
        class(very_long_int_t), intent(in) :: a 
        type(very_long_int_t), intent(in) :: b 
        type(very_long_int_t) :: ans 
        integer, allocatable :: tmp(:)
        integer :: i

        tmp = a%arr 
        i = 1
        
        do 
            if (b == i) exit 
            tmp = core_multiply_func(tmp, a%arr)
            i = i + 1
        end do 

        ans = tmp
    end function pow_func

end module euler_mi_m
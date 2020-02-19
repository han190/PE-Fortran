module euler_mi_m
    implicit none 
    private

    type, public :: very_long_int_t
        integer, allocatable, dimension(:) :: arr
        character(len=1) :: sgn
    contains 
        procedure, private :: init_char_sub, init_int_sub, init_arr_sub
        generic :: assignment(=) => init_char_sub, init_int_sub, init_arr_sub
        procedure, private :: equal_func, equal_int_func, equal_char_func
        generic :: operator(==) => equal_func, equal_int_func, equal_char_func
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
        integer, allocatable, dimension(:) :: iarr
    end type mtrx_t

    public :: to_long

contains

    subroutine re_alloc(arr, n)
        integer, allocatable, dimension(:), intent(inout) :: arr
        integer, intent(in) :: n

        if ( allocated(arr) ) then 
            deallocate(arr)
        end if 
        allocate( arr(1:n) )
    end subroutine re_alloc

    subroutine init_char_sub(this, chars)
        class(very_long_int_t), intent(inout) :: this
        character(len=*), intent(in) :: chars
        integer :: i

        select case ( chars(1:1) )
        case('+')

            this%sgn = '+'
            call re_alloc( this%arr, len(chars) - 1 )
            do i = 2, len(chars) 
                read ( chars(i:i), * ) this%arr(i - 1)
            end do 

        case('-')

            this%sgn = '-'
            call re_alloc( this%arr, len(chars) - 1 )
            do i = 2, len(chars) 
                read ( chars(i:i), * ) this%arr(i - 1)
            end do 

        case default 

            this%sgn = '+'
            call re_alloc( this%arr, len(chars) )
            do i = 1, len(chars) 
                read ( chars(i:i), * ) this%arr(i)
            end do 
            
        end select
    end subroutine init_char_sub

    subroutine init_arr_sub(this, arr)
        class(very_long_int_t), intent(inout) :: this
        integer, allocatable, dimension(:), intent(in) :: arr
        
        call re_alloc( this%arr, size(arr) )
        this%arr(:) = arr(:)
        this%sgn = '+'
    end subroutine init_arr_sub

    subroutine init_int_sub(this, int)
        class(very_long_int_t), intent(inout) :: this 
        integer, intent(in) :: int 
        integer :: tmp, i, l 

        if (int >= 0) then 
            this%sgn = '+'
        else if (int < 0) then 
            this%sgn = '-'
        end if 

        tmp = abs(int)
        l = floor( log10( real(tmp) ) ) + 1
        call re_alloc(this%arr, l)

        do i = l, 1, -1
            this%arr(i) = mod(tmp, 10)
            tmp = tmp / 10
        end do 
    end subroutine init_int_sub

    function to_long(chars) result(ans)
        character(len=*), intent(in) :: chars
        type(very_long_int_t) :: ans 

        call init_char_sub(ans, chars)
    end function to_long

    logical function equal_abs_val_func(arr1, arr2)
        integer, allocatable, dimension(:), intent(in) :: arr1, arr2

        equal_abs_val_func = .true. 

        if ( size(arr1) /= size(arr2) ) then 
            equal_abs_val_func = .false.
        else
            if ( all(arr1 == arr2) ) then 
                equal_abs_val_func = .true.
            else 
                equal_abs_val_func = .false.
            end if 
        end if 
    end function equal_abs_val_func

    logical function equal_func(a, b)
        class(very_long_int_t), intent(in) :: a 
        type(very_long_int_t), intent(in) :: b

        if ( a%sgn == b%sgn .and. equal_abs_val_func(a%arr, b%arr) ) then 
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
        equal_int_func = .false.

        if ( equal_func(a, tmp) ) then 
            equal_int_func = .true.
        end if 
    end function equal_int_func

    logical function equal_char_func(a, b)
        class(very_long_int_t), intent(in) :: a 
        character(len=*), intent(in) :: b 
        type(very_long_int_t) :: tmp 

        tmp = b
        equal_char_func = .false.

        if ( equal_func(a, tmp) ) then 
            equal_char_func = .true.
        end if 
    end function equal_char_func

    subroutine carry_sub(arr)
        integer, allocatable, dimension(:), intent(inout) :: arr
        integer, allocatable, dimension(:) :: tmp1, tmp2, tmp_arr

        allocate( &
            tmp1( size(arr) + 2 ), tmp2( size(arr) + 2 ), &
            tmp_arr( size(arr) + 2 ) &
        )

        tmp1(:) = 0; tmp2(:) = 0; tmp_arr(:) = 0
        tmp_arr(3:) = arr(:)

        do
            if ( all(tmp_arr <= 9) ) exit 
            tmp1(:) = tmp_arr(:) - tmp_arr(:) / 10 * 10
            tmp2( 1:size(tmp2) - 1 ) = tmp_arr(2:) / 10
            tmp_arr(:) = tmp1(:) + tmp2(:)
        end do 

        call move_alloc(tmp_arr, arr)
    end subroutine carry_sub

    subroutine cut_leading_zeros(arr)
        integer, allocatable, dimension(:), intent(inout) :: arr
        integer, allocatable, dimension(:) :: tmp
        integer :: i

        i = 1
        call move_alloc(arr, tmp)

        find_zeros: do
            if ( tmp(i) /= 0  .or. i >= size(tmp) ) exit find_zeros
            i = i + 1
        end do find_zeros

        allocate( arr( size(tmp(i:) ) ) )
        arr(:) = tmp(i:)
    end subroutine cut_leading_zeros

    logical function greater_abs_val_func(arr1, arr2)
        integer, allocatable, dimension(:), intent(in) :: arr1, arr2
        integer :: i 

        greater_abs_val_func = .false.

        if ( size(arr1) > size(arr2) ) then 
            greater_abs_val_func = .true.
        else if ( size(arr1) < size(arr2) ) then 
            greater_abs_val_func = .false.
        else
            greater_abs_val_func = .true.

            do i = 1, size(arr1)
                if ( arr1(i) < arr2(i) ) then 
                    greater_abs_val_func = .false.
                    return 
                end if 
            end do
        end if 
    end function greater_abs_val_func

    subroutine core_add_sub(arr1, arr2, ans)
        integer, allocatable, dimension(:), intent(in) :: arr1, arr2
        integer, allocatable, dimension(:), intent(out) :: ans
        integer, allocatable, dimension(:) :: tmp1, tmp2, tmp3

        associate( x => max( size(arr1), size(arr2) ) + 1 )
            allocate ( tmp1(x), tmp2(x), tmp3(x) )
            tmp1 = 0; tmp2 = 0; tmp3 = 0
            tmp1( x - size(arr1) + 1:x ) = arr1(:)
            tmp2( x - size(arr2) + 1:x ) = arr2(:)
        end associate

        tmp3(:) = tmp1(:) + tmp2(:) 
        call carry_sub(tmp3)
        call cut_leading_zeros(tmp3)
        call move_alloc(tmp3, ans)
    end subroutine core_add_sub

    subroutine core_subtract_func(arr1, arr2, ans)
        integer, allocatable, dimension(:), intent(in) :: arr1, arr2
        integer, allocatable, dimension(:), intent(out) :: ans
        integer, allocatable, dimension(:) :: tmp1, tmp2, tmp3
        integer :: i

        associate( x => max( size(arr1), size(arr2) ) + 1 )
            allocate( tmp1(x), tmp2(x), tmp3(x) )
            tmp1 = 0; tmp2 = 0; tmp3 = 0
            tmp1( x - size(arr1) + 1:x ) = arr1(:) 
            tmp2( x - size(arr2) + 1:x ) = arr2(:)
        end associate

        do i = size(tmp1), 2, -1
            if ( tmp1(i) >= tmp2(i) ) then 
                tmp3(i) = tmp1(i) - tmp2(i)
            else 
                tmp1(i) = tmp1(i) + 10
                tmp1(i - 1) = tmp1(i - 1) - 1
                tmp3(i) = tmp1(i) - tmp2(i)
            end if 
        end do 

        call cut_leading_zeros(tmp3)
        call move_alloc(tmp3, ans)
    end subroutine core_subtract_func

    function add_func(a, b) result(ans)
        class(very_long_int_t), intent(in) :: a
        type(very_long_int_t), intent(in) :: b
        type(very_long_int_t) :: ans

        if ( equal_abs_val_func(a%arr, b%arr) .and. a%sgn /= b%sgn ) then 

            ans = '0'
        
        else if (a%sgn == '+' .and. b%sgn == '+') then

            call core_add_sub(a%arr, b%arr, ans%arr)
            ans%sgn = '+'

        else if (a%sgn == '-' .and. b%sgn == '-') then 

            call core_add_sub(a%arr, b%arr, ans%arr)
            ans%sgn = '-'

        else if (a%sgn == '+' .and. b%sgn == '-') then 

            if ( greater_abs_val_func(a%arr, b%arr) ) then
                call core_subtract_func(a%arr, b%arr, ans%arr)
                ans%sgn = '+'
            else
                call core_subtract_func(b%arr, a%arr, ans%arr)
                ans%sgn = '-'
            end if 

        else if (a%sgn == '-' .and. b%sgn == '+') then 
            
            if ( greater_abs_val_func(a%arr, b%arr) ) then
                call core_subtract_func(a%arr, b%arr, ans%arr)
                ans%sgn = '-'
            else
                call core_subtract_func(b%arr, a%arr, ans%arr)
                ans%sgn = '+'
            end if 

        end if 

    end function add_func 

    function subtract_func(a, b) result(ans)
        class(very_long_int_t), intent(in) :: a
        type(very_long_int_t), intent(in) :: b
        type(very_long_int_t) :: ans

        if ( equal_abs_val_func(a%arr, b%arr) .and. a%sgn == b%sgn) then 

            ans = '0'

        else if (a%sgn == '+' .and. b%sgn == '+') then 

            if ( greater_abs_val_func(a%arr, b%arr) ) then 
                call core_subtract_func(a%arr, b%arr, ans%arr)
                ans%sgn = '+'
            else
                call core_subtract_func(b%arr, a%arr, ans%arr)
                ans%sgn = '-'
            end if 

        else if (a%sgn == '+' .and. b%sgn == '-') then 

                call core_add_sub(a%arr, b%arr, ans%arr)
                ans%sgn = '+'

        else if (a%sgn == '-' .and. b%sgn == '+') then 

                call core_add_sub(a%arr, b%arr, ans%arr)
                ans%sgn = '-'

        else if (a%sgn == '-' .and. b%sgn == '-') then 

            if ( greater_abs_val_func(a%arr, b%arr) ) then 
                call core_subtract_func(a%arr, b%arr, ans%arr)
                ans%sgn = '-'
            else
                call core_subtract_func(b%arr, a%arr, ans%arr)
                ans%sgn = '+'
            end if 

        end if 
    end function subtract_func

    subroutine core_multiply_func(arr1, arr2, ans)
        integer, allocatable, dimension(:), intent(in) :: arr1, arr2
        integer, allocatable, dimension(:), intent(out) :: ans
        type(mtrx_t), allocatable, dimension(:) :: mtrx
        integer, allocatable, dimension(:) :: tmp
        integer :: i

        associate( x => size(arr1) + size(arr2), y => size(arr2) )
            allocate( mtrx(y) )
            
            do i = 1, y
                allocate( mtrx(i)%iarr(x) )

                tmp = arr1(:) * arr2( size(arr2) - i + 1 )
                call carry_sub(tmp) ! could be optimized here.
                call cut_leading_zeros(tmp)
                associate( row => mtrx(i)%iarr(:) )
                    row = 0
                    row( &
                        size(row) - size(tmp) - i + 2: &
                        size(row) - i + 1 &
                    ) = tmp(:)
                end associate
            end do 

            call re_alloc(tmp, size( mtrx(1)%iarr(:) ) )
            tmp(:) = 0 ! Initialize array tmp
            do i = 1, y 
                tmp(:) = tmp(:) + mtrx(i)%iarr(:)
            end do 

            call carry_sub(tmp)
            call cut_leading_zeros(tmp)
            call move_alloc(tmp, ans)
        end associate 
    end subroutine core_multiply_func

    function multiply_func(a, b) result(ans)
        class(very_long_int_t), intent(in) :: a 
        type(very_long_int_t), intent(in) :: b 
        type(very_long_int_t) :: ans 

        if ( &
            (a%sgn == '+' .and. b%sgn == '+') .or. &
            (a%sgn == '-' .and. b%sgn == '-') &
        ) then 

            call core_multiply_func(a%arr, b%arr, ans%arr)
            ans%sgn = '+'

        else

            call core_multiply_func(a%arr, b%arr, ans%arr)
            ans%sgn = '-'

        end if 
    end function multiply_func

    function pow_func(a, b) result(ans)
        class(very_long_int_t), intent(in) :: a
        integer, intent(in) :: b
        type(very_long_int_t) :: ans, tmp 
        integer :: i

        if (b == 0) then 
            ans = '1'
            return 
        end if 
        
        i = 1
        tmp = a

        do
            if (i == b) then 
                exit 
            else
                tmp = a * tmp 
                i = i + 1 
            end if 
        end do 

        ans = tmp
    end function pow_func

end module euler_mi_m
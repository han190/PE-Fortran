module euler_mp_m
    implicit none 
    private

    type, public :: long_int_t
        integer, allocatable :: arr(:)
    contains 
        procedure, private :: init_char_sub, init_int_sub, init_arr_sub
        generic :: assignment(=) => init_char_sub, init_int_sub, init_arr_sub
        procedure, private :: equal_func, equal_int_func
        generic :: operator(==) => equal_func, equal_int_func
        procedure, private :: add_func
        generic :: operator(+) => add_func 
        procedure, private :: multiply_func
        generic :: operator(*) => multiply_func
        procedure, private :: pow_func
        generic :: operator(**) => pow_func
        procedure :: mod2 => mod2_sub
    end type long_int_t 

    type :: mtrx_t
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
        class(long_int_t), intent(inout) :: this
        character(len=*), intent(in) :: char
        integer :: i

        call re_alloc( this%arr, len(char) )
        do i = 1, len(char) 
            read ( char(i:i), * ) this%arr(i)
        end do 
    end subroutine init_char_sub

    subroutine init_arr_sub(this, arr)
        class(long_int_t), intent(inout) :: this
        integer, allocatable, intent(in) :: arr(:)
        
        call re_alloc( this%arr, size(arr) )
        this%arr = arr
    end subroutine init_arr_sub

    subroutine init_int_sub(this, int)
        class(long_int_t), intent(inout) :: this 
        integer, intent(in) :: int 
        integer :: tmp, i, l 

        tmp = int 
        l = floor( log10( real(int) ) ) + 1

        call re_alloc(this%arr, l)

        do i = l, 1, -1
            this%arr(i) = mod(tmp, 10)
            tmp = tmp / 10
        end do 
    end subroutine init_int_sub

    logical function equal_func(a, b)
        class(long_int_t), intent(in) :: a 
        type(long_int_t), intent(in) :: b 

        if ( all(a%arr == b%arr) ) then 
            equal_func = .true.
        else 
            equal_func = .false. 
        end if 
    end function equal_func

    logical function equal_int_func(a, b)
        class(long_int_t), intent(in) :: a 
        integer, intent(in) :: b 
        type(long_int_t) :: tmp 

        tmp = b 
        if (a == tmp) then
            equal_int_func = .true.
        else 
            equal_int_func = .false.
        end if 
    end function equal_int_func

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
        tmp1(3:) = arr - arr/10*10
        tmp2(1) = 0
        tmp2( size(tmp2) ) = 0 
        tmp2( 2:size(tmp2) - 1 ) = arr/10
        arr = tmp1 + tmp2 

        do
            if ( all(arr <= 9) ) exit 
            tmp1 = arr - arr/10*10
            tmp2(1:size(tmp2) - 1) = arr(2:)/10
            arr = tmp1 + tmp2
        end do 
    end subroutine carry_sub

    function add_func(a, b) result(ans)
        class(long_int_t), intent(in) :: a
        type(long_int_t), intent(in) :: b
        type(long_int_t) :: ans
        integer, allocatable :: tmp1(:), tmp2(:), tmp3(:) 

        associate(                                                             &
            x => max( size(a%arr), size(b%arr) ) + 1                           &
        )
            allocate( tmp1(x), tmp2(x), tmp3(x) )
            tmp1 = 0; tmp2 = 0; tmp3 = 0
            tmp1( x - size(a%arr) + 1:x ) = a%arr 
            tmp2( x - size(b%arr) + 1:x ) = b%arr 
        end associate

        tmp3 = tmp1 + tmp2 
        call carry_sub(tmp3)
        call cut_leading_zeros(tmp3)
        ans = tmp3
    end function add_func 

    function multiply_core_func(arr1, arr2) result(ans)
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
    end function multiply_core_func

    function multiply_func(a, b) result(ans)
        class(long_int_t), intent(in) :: a 
        type(long_int_t), intent(in) :: b 
        type(long_int_t) :: ans 

        ans = multiply_core_func(a%arr, b%arr)
    end function multiply_func

    function pow_func(a, b) result(ans)
        class(long_int_t), intent(in) :: a 
        type(long_int_t), intent(in) :: b 
        type(long_int_t) :: ans 
        integer, allocatable :: tmp(:)
        integer :: i

        tmp = a%arr 
        i = 1
        
        do 
            if (b == i) exit 
            tmp = multiply_core_func(tmp, a%arr)
            i = i + 1
        end do 

        ans = tmp
    end function pow_func

    subroutine cohere10(arr)
        integer, allocatable, intent(inout) :: arr(:)
        integer :: i

        do i = 1, size(arr) - 1
            if ( arr(i) == 1 ) then 
                arr(i) = 0 
                arr(i + 1) = arr(i + 1) + 10
            end if 
        end do 
    end subroutine cohere10

    subroutine mod2_sub(this, ans, res)
        class(long_int_t) :: this 
        type(long_int_t), intent(out) :: ans, res
        integer, allocatable :: tmp_res(:), tmp_ans(:)

        call re_alloc( tmp_ans, size(this%arr) )
        call re_alloc( tmp_res, size(this%arr) )

        tmp_res = this%arr
        tmp_ans = 0
        do
            call cohere10(tmp_res)
            tmp_ans = tmp_ans + tmp_res / 2
            tmp_res = tmp_res - tmp_res / 2 * 2
            
            if (                                                               &
                all(tmp_res == 0) .or.                                         &
                (                                                              &
                    all( tmp_res( :size(tmp_res) - 1 ) == 0 ) .and.            &
                    tmp_res( size(tmp_res) ) == 1                              &
                )                                                              &
            ) exit 
        end do 

        call carry_sub(tmp_res)
        call cut_leading_zeros(tmp_res)
        res = tmp_res

        call carry_sub(tmp_ans)
        call cut_leading_zeros(tmp_ans)
        ans = tmp_ans
    end subroutine mod2_sub

end module euler_mp_m
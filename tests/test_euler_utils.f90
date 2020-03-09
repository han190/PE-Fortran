program test_euler_utils
    use iso_fortran_env, only: int64
    use euler_utils_m
    implicit none 

    ! Conditions
    logical :: cond

    !func_name = ''
    test_unit_digit: block
        integer :: i
        integer(int64) :: i_long
        character(10) :: func_name

        func_name = 'unit_digit'
        i = 1120
        i_long = 1120_int64

        cond = &
            unit_digit(i) == 0 .and. &
            unit_digit(i_long) == 0_int64
        
        call if_passed(cond, func_name)
    end block test_unit_digit

    test_swap: block 
        integer :: i, j
        integer(int64) :: i_long, j_long 
        real(sp) :: a_sp, b_sp
        real(dp) :: a_dp, b_dp
        character(4) :: func_name

        func_name = 'swap'
        i = 1120
        j = 47

        i_long = 1120_int64
        j_long = 47_int64

        a_sp = 11.20_sp
        b_sp = 4.7_sp 

        a_dp = 11.20_dp 
        b_dp = 4.7_dp

        call swap(i, j)
        call swap(i_long, j_long)
        call swap(a_sp, b_sp)
        call swap(a_dp, b_dp)

        cond = &
            i == 47 .and. j == 1120 .and. &
            i_long == 47_int64 .and. j_long == 1120_int64 .and. &
            abs(a_sp - 4.7_sp) < tiny_sp .and. &
            abs(b_sp - 11.20_sp) < tiny_sp .and. &
            abs(a_dp - 4.7_dp) < tiny_dp .and. &
            abs(b_dp - 11.20_dp) < tiny_dp

        call if_passed(cond, func_name)
    end block test_swap 

    test_digs_of_int: block 
        integer :: i
        integer(int64) :: i_long 
        character(11) :: func_name

        func_name = 'digs_of_int'
        i = 1120
        i_long = 1120_int64

        cond = &
            digs_of_int(i) == 4 .and. &
            digs_of_int(i_long) == 4_int64
        
        call if_passed(cond, func_name)
    end block test_digs_of_int

    test_fibonacci: block 
        integer :: i
        integer, allocatable :: fibonacci_arr(:)
        integer(int64) :: i_long 
        integer(int64), allocatable :: fibonacci_arr_long(:)
        character(9) :: func_name

        func_name = "fibonacci"
        fibonacci_arr = [ &
            0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, &
            89, 144, 233, 377, 610, 987, 1597, &
            2584, 4181, 6765, 10946, 17711, 28657, &
            46368, 75025, 121393, 196418, 317811 &
        ]

        cond = .true.
        do i = 1, size(fibonacci_arr) - 1
            cond = cond .and. &
                fibonacci(i) == fibonacci_arr(i + 1)
        end do 

        fibonacci_arr_long = [ &
            0_int64, 1_int64, 1_int64, 2_int64, &
            3_int64, 5_int64, 8_int64, 13_int64, &
            21_int64, 34_int64, 55_int64, &
            89_int64, 144_int64, 233_int64, &
            377_int64, 610_int64, 987_int64, &
            1597_int64, 2584_int64, 4181_int64, &
            6765_int64, 10946_int64, 17711_int64, &
            28657_int64, 46368_int64, 75025_int64, &
            121393_int64, 196418_int64, 317811_int64 &
        ]

        do i_long = 1_int64, size(fibonacci_arr_long) - 1_int64
            cond = cond .and. &
                fibonacci(i_long) == &
                fibonacci_arr(i_long + 1_int64)
        end do 

        call if_passed(cond, func_name)
    end block test_fibonacci

    test_reverse: block 
        integer :: i
        integer(int64) :: i_long
        character(7) :: func_name 

        func_name = 'reverse'
        i = 1234
        i_long = 1234567_int64 

        cond = &
            reverse(i) == 4321 .and. &
            reverse(i_long) == 7654321_int64

        call if_passed(cond, func_name)
    end block test_reverse

    test_is_palindromic: block
        integer :: i, j
        integer(int64) :: i_long, j_long
        character(14) :: func_name

        func_name = 'is_palindromic'

        i = 12321
        j = 12345

        i_long = 123454321_int64
        j_long = 123456789_int64

        cond = &
            is_palindromic(i) .and. &
            ( .not. is_palindromic(j) ) .and. &
            is_palindromic(i_long) .and. &
            ( .not. is_palindromic(j_long) )
        
        call if_passed(cond, func_name)
    end block test_is_palindromic

    test_gcd: block
        integer :: i, j 
        integer(int64) :: i_long, j_long
        character(3) :: func_name

        func_name = 'gcd'
        i = 54
        j = 24

        i_long = 54_int64
        j_long = 24_int64

        cond = &
            gcd(i, j) == 6 .and. &
            gcd(i_long, j_long) == 6_int64 
        
        call if_passed(cond, func_name)
    end block test_gcd

    test_lcm: block 
        integer :: i, j 
        integer(int64) :: i_long, j_long
        character(3) :: func_name

        func_name = 'lcm'
        i = 21
        j = 6 

        i_long = 21_int64 
        j_long = 6_int64

        cond = &
            lcm(i, j) == 42 .and. &
            lcm(i_long, j_long) == 42_int64

        call if_passed(cond, func_name)
    end block test_lcm

    test_factorial: block 
        integer :: i
        integer, allocatable :: factorial_arr(:)
        integer(int64), allocatable :: factorial_arr_long(:)
        character(9) :: func_name

        factorial_arr = [ &
            1, 1, 2, 6, 24, 120, 720, &
            5040, 40320, 362880, &
            3628800, 39916800 &
        ]

        func_name = 'factorial'
        cond = .true.
        do i = 1, size(factorial_arr) - 1
            cond = cond .and. &
                factorial(i) == factorial_arr(i + 1)
        end do 

        factorial_arr_long = [ &
            1_int64, 1_int64, 2_int64, 6_int64, &
            24_int64, 120_int64, 720_int64, &
            5040_int64, 40320_int64, 362880_int64, &
            3628800_int64, 39916800_int64 &
        ]

        do i = 1, size(factorial_arr_long) - 1
            cond = cond .and. &
                factorial(i) == factorial_arr_long(i + 1)
        end do 

        call if_passed(cond, func_name)
    end block test_factorial

    test_is_pandigital: block
        integer :: i
        integer(int64) :: i_long 
        character(13) :: func_name

        ! Pandigital number: from 1 to 9
        i = 15342769
        i_long = 153427698_int64

        func_name = 'is_pandigital'
        cond = &
            ( .not. is_pandigital(i) ) .and. &
            is_pandigital(i_long)
        

        call if_passed(cond, func_name)
    end block test_is_pandigital

    test_int_2_arr: block 
        integer :: i
        integer(int64) :: i_long
        integer, allocatable :: i_arr(:), arr(:)
        integer, allocatable :: i_arr_long(:), arr_long(:)
        character(9) :: func_name

        i = 12345
        i_arr = [1, 2, 3, 4, 5]

        i_long = 1234567_int64
        i_arr_long = [1, 2, 3, 4, 5, 6, 7]

        func_name = 'int_2_arr'

        call int_2_arr( i, arr )
        call int_2_arr( i_long, arr_long )
        
        cond = .true. 
        do i = 1, size(i_arr)
            cond = cond .and. &
                arr(i) == i_arr(i)
        end do 

        do i = 1, size(i_arr_long)
            cond = cond .and. &
                arr_long(i) == i_arr_long(i)
        end do 

        call if_passed(cond, func_name)
    end block test_int_2_arr

    test_arr_2_int: block 
        integer :: i, j
        integer(int64) :: i_long, j_long
        integer, allocatable :: arr(:)
        integer(int64), allocatable :: arr_long(:)
        character(9) :: func_name

        i = 12345
        arr = [1, 2, 3, 4, 5]

        i_long = 1234567890_int64
        arr_long = [1, 2, 3, 4, 5, 6, 7, 8, 9, 0]

        call arr_2_int(arr, j)
        call arr_2_int(arr_long, j_long)

        func_name = 'arr_2_int'
        cond = i == j .and. i_long == j_long

        call if_passed(cond, func_name)
    end block test_arr_2_int

    test_append: block
        integer, allocatable :: iarr(:)
        integer(int64), allocatable :: iarr_long(:)
        real(sp), allocatable :: arr_sp(:)
        real(dp), allocatable :: arr_dp(:)
        character(6) :: func_name

        iarr = [1, 2, 3]
        iarr_long = [1_int64, 2_int64, 3_int64]
        arr_sp = [1._sp, 2._sp, 3._sp]
        arr_dp = [1._dp, 2._dp, 3._dp]

        call append(iarr, 4)
        call append(iarr_long, 4_int64)
        call append(arr_sp, 4._sp)
        call append(arr_dp, 4._dp)

        func_name = 'append'
        cond = &
            all(iarr == [1, 2, 3, 4]) .and. &
            all(iarr_long == [1_int64, 2_int64, 3_int64, 4_int64])

        call if_passed(cond, func_name)
    end block test_append

contains 

    subroutine testing(test_msg)
        character(len=*), intent(in) :: test_msg 

        print '(a)', "Testing '"//test_msg//"' ..."
    end subroutine testing 

    subroutine error_stop(err_msg)
        character(len=*), intent(in) :: err_msg

        print '(a)', "Testing '"//err_msg//"' failed."
        error stop 
    end subroutine error_stop

    subroutine if_passed(logical_cond, func_name)
        logical, intent(in) :: logical_cond 
        character(len=*), intent(in) :: func_name 

        call testing(func_name)
        if (logical_cond) then 
            print '(a)', "Test passed."
        else 
            call error_stop(func_name)
        end if 
    end subroutine if_passed

end program test_euler_utils
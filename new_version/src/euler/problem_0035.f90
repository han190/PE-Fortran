submodule(interface_m) euler_problem_0035_m
    implicit none

contains

    module character(len=20) function euler0035()
        write (euler0035, "(i20)") answer()
    end function euler0035

    pure integer(i32) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i32), parameter :: n = 1000000
        logical :: is_circular, is_circulars(n)
        integer(i32), allocatable :: array(:)
        logical, allocatable :: is_prime(:)
        integer(i32) :: i

        call Sieve_of_Eratosthenes(n, is_prime)
        is_circulars = .false.

        do i = 100, n
            if (is_circulars(i)) cycle
            call is_circular_prime(i, is_prime, array, is_circular)
            if (is_circular) is_circulars(array) = .true.
        end do
        answer = count(is_circulars) + 13
    end function answer

    pure subroutine is_circular_prime(n, is_prime, array, is_circular)
        integer(i32), intent(in) :: n
        logical, intent(in) :: is_prime(:)
        integer(i32), allocatable, intent(out) :: array(:)
        logical, intent(out) :: is_circular
        integer(i32) :: temp

        is_circular = .true.
        if (.not. is_prime(n)) then
            is_circular = .false.
            return
        end if

        temp = rotate(n)
        do
            if (temp == n) exit
            if (.not. is_prime(temp)) then
                is_circular = .false.
                return
            end if
            temp = rotate(temp)
        end do

        allocate (array(number_of_digits(n)))
        call circular_array(n, array)
    end subroutine is_circular_prime

    pure subroutine circular_array(n, array)
        integer(i32), intent(in) :: n
        integer(i32), intent(out) :: array(:)
        integer(i32) :: i, temp

        temp = n
        do i = 1, number_of_digits(n)
            array(i) = temp
            temp = rotate(temp)
        end do
    end subroutine circular_array

    pure integer(i32) function rotate(n)
        integer(i32), intent(in) :: n

        rotate = unit_digit(n)*10**(number_of_digits(n) - 1) + n/10
    end function rotate

end submodule euler_problem_0035_m
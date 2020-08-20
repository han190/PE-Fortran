submodule(euler_interface_m) euler_prob_0035_m
    implicit none
    logical, allocatable :: is_prime(:)

contains

    module character(len=20) function euler0035()
        write (euler0035, "(i20)") ans(1000000)
    end function euler0035

    integer function ans(n)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none

        integer, intent(in) :: n
        logical :: logical_arr(n), is_c
        integer, allocatable :: arr(:)
        integer :: i

        call sieve_of_Eratosthenes(n, is_prime)
        logical_arr = .false.

        do i = 1, n
            if (logical_arr(i)) cycle
            call is_circular_prime(i, arr, is_c)

            if (is_c) then
                logical_arr(arr) = .true.
            end if
        end do

        ans = count(logical_arr)
    end function ans

    subroutine is_circular_prime(n, arr, is_circular)
        integer, intent(in) :: n
        integer, allocatable, intent(out) :: arr(:)
        logical, intent(out) :: is_circular
        integer :: tmp

        is_circular = .true.

        if (.not. is_prime(n)) then
            is_circular = .false.
            return
        end if

        tmp = rot_int(n)

        loop: do
            if (tmp == n) exit
            if (.not. is_prime(tmp)) then
                is_circular = .false.
                return
            end if
            tmp = rot_int(tmp)
        end do loop

        call rot_int_arr(n, arr)
    end subroutine is_circular_prime

    subroutine rot_int_arr(n, arr)
        integer, intent(in) :: n
        integer, allocatable, intent(out) :: arr(:)
        integer :: i, tmp

        allocate (arr(digs_of_int(n)))
        tmp = n; arr(1) = tmp

        do i = 2, size(arr)
            tmp = rot_int(tmp)
            arr(i) = tmp
        end do
    end subroutine rot_int_arr

    integer function rot_int(n)
        integer, intent(in) :: n
        integer :: tmp

        tmp = 10**(digs_of_int(n) - 1)
        rot_int = tmp*unit_digit(n) + n/10
    end function rot_int

end submodule euler_prob_0035_m

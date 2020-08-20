submodule(euler_interface_m) euler_prob_0049_m
    implicit none

contains

    character(len=20) function euler0049()
        write (euler0049, "(a20)") trim(ans())
    end function euler0049

    character(len=20) function ans()
        use euler_primes_m, only: sieve_of_Eratosthenes
        integer :: i, a, b, c
        character(4) :: str1, str2, str3
        logical :: cond
        logical, allocatable :: is_prime(:)

        call sieve_of_Eratosthenes(10000, is_prime)

        do i = 9973, 7661, -1
            a = i
            b = a - 3330
            c = b - 3330

            cond = &
                is_prime(a) .and. &
                is_prime(b) .and. &
                is_prime(c)

            if (.not. cond) cycle

            call sort_int(a)
            call sort_int(b)
            call sort_int(c)

            if (a == b .and. a == c) exit
        end do

        write (str1, "(i4)") i
        write (str2, "(i4)") i - 3330
        write (str3, "(i4)") i - 6660

        ans = str3//str2//str1
    end function ans

    subroutine sort_int(n)
        integer, intent(inout) :: n
        character(4) :: str

        write (str, "(i4)") n
        call sort_str4(str)
        read (str, "(i4)") n
    end subroutine sort_int

    subroutine sort_str4(str)
        character(4), intent(inout) :: str
        integer :: i, j

        do i = 1, 4
            do j = i + 1, 4
                if (str(i:i) > str(j:j)) then
                    call swap_str(str(i:i), str(j:j))
                end if
            end do
        end do
    end subroutine sort_str4

    subroutine swap_str(a, b)
        character(1), intent(inout) :: a, b
        character(1) :: tmp

        tmp = a
        a = b
        b = tmp
    end subroutine swap_str

end submodule euler_prob_0049_m

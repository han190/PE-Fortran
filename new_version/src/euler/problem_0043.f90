submodule(interface_m) euler_problem_0043_m
    implicit none
    integer(i64) :: sum_

contains

    module character(len=20) function euler0043()
        write (euler0043, "(i20)") answer()
    end function euler0043

    integer(i64) function answer()
        integer(i64) :: i, array(10)

        array = [(i, i=0, 9)]
        call permute(array, 1_i64, 10_i64)
        answer = sum_
    end function answer

    recursive subroutine permute(a, l, r)
        integer(i64), intent(inout) :: a(:)
        integer(i64), intent(in) :: l, r
        integer(i64) :: i

        if (l == r) then
            associate (temp => to_integer(a))
                if (is_divisible(temp)) sum_ = sum_ + temp
            end associate
            return
        else
            do i = l, r
                call swap(a(i), a(l))
                call permute(a, l + 1, r)
                call swap(a(i), a(l))
            end do
        end if
    end subroutine permute

    pure logical function is_divisible(n)
        integer(i64), intent(in) :: n
        integer(i64) :: primes(7), temp, i, x

        primes = [17, 13, 11, 7, 5, 3, 2]
        temp = n

        is_divisible = .true.
        do i = 1, 7
            x = mod(temp, 1000_i64)
            if (mod(x, primes(i)) /= 0) then
                is_divisible = .false.
                return
            end if
            temp = temp/10
        end do
    end function is_divisible

    pure subroutine swap(a, b)
        integer(i64), intent(inout) :: a, b
        integer(i64) :: temp

        temp = a; a = b; b = temp
    end subroutine swap

end submodule euler_problem_0043_m

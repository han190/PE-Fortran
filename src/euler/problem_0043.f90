submodule(interface_m) euler_problem_0043_m
    implicit none

contains

    module character(len=20) function euler0043()
        write (euler0043, "(i20)") answer()
    end function euler0043

    integer(i64) function answer()
        integer(i64) :: i, array(10)
        logical :: next_avail

        array = [(i, i=0, 9)]
        answer = 0
        next_avail = .true.

        do while (next_avail)
            associate (temp => (to_integer(array)))
                if (is_divisible(temp)) answer = answer + temp
            end associate
            call permute(array, next_avail)
        end do
    end function answer

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

end submodule euler_problem_0043_m

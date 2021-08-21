submodule(euler_interface_m) euler_prob_0007_m
    implicit none

contains

    module character(len=20) function euler0007()
        write (euler0007, "(i20)") answer(10000)
    end function euler0007

    pure function answer(n) result(i)
        use euler_primes_m, only: is_prime
        implicit none
        integer, intent(in) :: n
        integer :: i, j

        i = 0; j = 0
        loop_1: do
            if (j == n + 1) exit loop_1
            i = i + 1
            if (mod(i, 2) == 2 .and. i /= 2) then
                cycle loop_1
            else if (is_prime(int(i))) then
                j = j + 1
            end if
        end do loop_1
    end function answer

end submodule euler_prob_0007_m

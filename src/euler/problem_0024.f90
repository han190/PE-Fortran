submodule(interface_m) euler_problem_0024_m
    implicit none

contains

    module character(len=20) function euler0024()
        write (euler0024, "(i20)") answer()
    end function euler0024

    elemental integer(i64) function answer()
        integer(i64) :: factors(10)
        integer(i64), allocatable :: temp(:)
        integer(i64) :: n, i, j

        factors = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
        temp = [(i, i=0, 9)]
        n = 999999
        answer = 0

        do i = 10, 1, -1
            j = temp(n/factors(i) + 1)
            n = mod(n, factors(i))
            temp = pack(temp, temp /= j)
            answer = answer + j*10**(i - 1)
        end do
    end function answer

end submodule euler_problem_0024_m

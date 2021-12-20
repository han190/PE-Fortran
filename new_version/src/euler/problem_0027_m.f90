submodule(interface_m) euler_problem_0027_m
    implicit none

contains

    module character(len=20) function euler0027()
        write (euler0027, "(i20)") answer()
    end function euler0027

    pure integer(i32) function answer()
        answer = -(2*30 + 1)*quadratic_primes(1, 41, 30)
    end function answer

    pure integer(i32) function quadratic_primes(i, j, n)
        integer, intent(in) :: i, j, n

        if (abs(i) >= 1000 .or. abs(j) > 1000) then
            quadratic_primes = 0
        else
            quadratic_primes = n**2 + i*n + j
        end if
    end function quadratic_primes

end submodule euler_problem_0027_m

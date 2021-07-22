submodule(euler_interface_m) euler_prob_0027_m
    implicit none

contains

    module character(len=20) function euler0027()
        write (euler0027, "(i20)") ans()
    end function euler0027

    integer function ans()
        integer :: a, b

        a = -(2*30 + 1)
        b = quadratic_primes(1, 41, 30)
        ans = a*b
    end function ans

    integer function quadratic_primes(i, j, n)
        integer, intent(in) :: i, j, n

        if (abs(i) >= 1000 .or. abs(j) > 1000) then
            quadratic_primes = 0
            return
        end if
        quadratic_primes = n**2 + i*n + j
    end function quadratic_primes

end submodule euler_prob_0027_m

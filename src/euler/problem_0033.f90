submodule(interface_m) euler_problem_0033_m
    implicit none

contains

    module character(len=20) function euler0033()
        write (euler0033, "(i20)") answer()
    end function euler0033

    elemental integer(i32) function answer()
        use stdlib_math, only: gcd
        implicit none

        integer(i32) :: denom, numer
        integer(i32) :: counter
        integer(i32) :: denom_prod, numer_prod

        denom = 1; numer = 1
        denom_prod = 1; numer_prod = 1

        do counter = 1, 9
            do denom = 1, counter - 1
                do numer = 1, denom - 1
                    if ((numer*10 + counter)*denom == &
                        (counter*10 + denom)*numer) then
                        denom_prod = denom_prod*denom
                        numer_prod = numer_prod*numer
                    end if
                end do
            end do
        end do
        answer = denom_prod/gcd(numer_prod, denom_prod)
    end function answer

end submodule euler_problem_0033_m

submodule(interface_m) euler_problem_0033_m
    implicit none

contains

    module character(len=20) function euler0033()
        write (euler0033, "(i20)") answer()
    end function euler0033

    pure integer(i32) function answer()
        integer(i32) :: denominator, numerator
        integer(i32) :: counter
        integer(i32) :: denominator_product, numerator_product

        denominator = 1; numerator = 1
        denominator_product = 1; numerator_product = 1

        do counter = 1, 9
            do denominator = 1, counter - 1
                do numerator = 1, denominator - 1
                    if ((numerator*10 + counter)*denominator == &
                        (counter*10 + denominator)*numerator) then
                        denominator_product = denominator_product*denominator
                        numerator_product = numerator_product*numerator
                    end if
                end do
            end do
        end do
        answer = denominator_product/gcd(numerator_product, denominator_product)
    end function answer

end submodule euler_problem_0033_m

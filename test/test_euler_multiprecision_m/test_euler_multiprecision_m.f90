program main

    use euler_multiprecision_m
    implicit none

    type(multiprecision_int_t) :: a, b, c
    integer :: p

    print "(a)", new_line("a")//"Test multiple precision integer type"
    test_addition: block
        a = "23459872394587023948572394857923457923458723455432"
        b = "23409587239485723489572934857293450234985723457632"
        c = a + b

        print "(a)", "(a) addition"
        print "(a, a1, 50(i1))", "a         = ", a%sgn, a%arr
        print "(a, a1, 50(i1))", "b         = ", b%sgn, b%arr
        print "(a, a1, 50(i1))", "c = a + b = ", c%sgn, c%arr
    end block test_addition

    test_subtraction: block
        a = "23459872394587023948572394857923457923458723455432"
        b = "23409587239485723489572934857293450234985723457632"
        c = a - b

        print "(a)", "(b) subtraction"
        print "(a, a1, 50(i1))", "a         = ", a%sgn, a%arr
        print "(a, a1, 50(i1))", "b         = ", b%sgn, b%arr
        print "(a, a1, 50(i1))", "c = a - b = ", c%sgn, c%arr
    end block test_subtraction

    test_multiplication: block
        a = "23459872394587023948572394857923457923458723455432"
        b = "23409587239485723489572934857293450234985723457632"
        c = a*b

        print "(a)", "(c) multiplication"
        print "(a, a1, 50(i1))", "a         = ", a%sgn, a%arr
        print "(a, a1, 50(i1))", "b         = ", b%sgn, b%arr
        print "(a, a1, 50(i1))", "c = a * b = ", c%sgn, c%arr
    end block test_multiplication

    test_power: block
        a = "23459872394587023948572394857923457923458723455432"
        p = 12
        c = a**p

        print "(a)", "(d) power"
        print "(a, a1, 50(i1))", "a         = ", a%sgn, a%arr
        print "(a, i4)", "p         = ", p
        print "(a, a1, 50(i1))", "c = a**p  = ", c%sgn, c%arr
    end block test_power

end program main

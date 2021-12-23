submodule(interface_m) euler_problem_0016_m
    implicit none

contains

    module character(len=20) function euler0016()
        write (euler0016, "(i20)") answer()
    end function euler0016

    !> There are faster ways to do this, but with an already
    !> implemented multiprecision integer library, this is
    !> the most cost-efficient way.
    pure integer(i32) function answer()
        use big_integer_m
        implicit none
        type(big_integer) :: x

        x = big_(2)**1000
        answer = sum(x%arr)
    end function answer

end submodule euler_problem_0016_m

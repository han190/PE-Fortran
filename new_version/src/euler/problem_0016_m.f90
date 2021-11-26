submodule(interface_m) euler_problem_0016_m
    implicit none

contains

    module pure character(len=20) function euler0016()
        write (euler0016, "(i20)") answer()
    end function euler0016

    !> There are faster ways to do this, but with an already
    !> implemented multiprecision integer library, this is
    !> the most cost-efficient way.
    pure integer(i32) function answer()
        use multiprecision_m, only: multiprecision_t, to_long
        implicit none
        type(multiprecision_t) :: long_integer

        long_integer = to_long(2)**1000
        answer = sum(long_integer%arr)
    end function answer

end submodule euler_problem_0016_m

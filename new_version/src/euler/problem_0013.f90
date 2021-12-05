submodule(interface_m) euler_problem_0013_m
    use multiprecision_m
    implicit none

contains

    module pure character(len=20) function euler0013()
        type(multiprecision_t) :: value_
        value_ = answer()
        write (euler0013, "(10(' '), 10(i1))") value_%arr(1:10)
    end function euler0013

    pure type(multiprecision_t) function answer()
        use data_m, only: get_euler_data_0013
        implicit none

        integer(i32) :: i
        type(multiprecision_t) :: temp
        character(len=:), allocatable :: euler_data(:)

        call get_euler_data_0013(euler_data)
        temp%sgn = "+"; allocate(temp%arr(50))
        answer = 0

        do i = 1, 100
            read (euler_data(i), "(50(i1))") temp%arr
            answer = answer + temp
        end do
    end function answer

end submodule euler_problem_0013_m
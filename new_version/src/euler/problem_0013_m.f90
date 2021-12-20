submodule(interface_m) euler_problem_0013_m
    use big_integer_m
    implicit none

contains

    module character(len=20) function euler0013()
        type(big_integer) :: value_
        value_ = answer()
        write (euler0013, "(10(' '), 10(i1))") value_%arr(1:10)
    end function euler0013

    type(big_integer) function answer()
        integer(i32) :: i, iunit
        type(big_integer) :: temp

        open (newunit=iunit, file=data_path//"/"//"data_0013.txt", &
              status="old", action="read")

        temp%sgn = .true.; allocate (temp%arr(50))
        answer = 0
        do i = 1, 100
            read (iunit, "(50(i1))") temp%arr
            answer = answer + temp
        end do
        close (iunit)
    end function answer

end submodule euler_problem_0013_m

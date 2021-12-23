submodule(interface_m) euler_problem_0038_m
    implicit none

contains

    module character(len=20) function euler0038()
        write (euler0038, "(i20)") answer()
    end function euler0038

    elemental integer(i64) function answer()
        integer(i64) :: i, j, temp

        answer = 0
        outer: do i = 2, 10000
            j = 1; temp = 0

            inner: do
                if (temp /= 0) then
                    if (number_of_digits(temp) > 9) cycle outer
                end if

                if (is_pandigital(temp)) then
                    if (temp > answer) answer = temp
                end if

                temp = i*j + temp*10**number_of_digits(i*j)
                j = j + 1
            end do inner
        end do outer
    end function answer

end submodule euler_problem_0038_m

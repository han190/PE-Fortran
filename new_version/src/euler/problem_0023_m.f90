submodule(interface_m) euler_problem_0023_m
    implicit none

contains

    module character(len=20) function euler0023()
        write (euler0023, "(i20)") answer()
    end function euler0023

    pure integer(i32) function answer()
        integer(i32), allocatable :: abundant(:)
        integer(i32), parameter :: min_ = 12, max_ = 28123
        integer(i32) :: i, j
        logical, allocatable :: sum_of_abundants(:)

        associate (indices => [(i, i=min_, max_)])
            abundant = pack(indices, mask=[(is_abundant(i), i=min_, max_)])
        end associate

        sum_of_abundants = [(.false., i=1, max_)]
        do i = 1, size(abundant)
            do j = i, size(abundant)
                associate (x => abundant(i) + abundant(j))
                    if (x <= max_) sum_of_abundants(x) = .true.
                end associate
            end do
        end do
        answer = sum([(i, i=1, max_)], mask=.not. sum_of_abundants)
    end function answer

    pure logical function is_abundant(val)
        integer(i32), intent(in) :: val

        is_abundant = .false.
        if (spd(val) > val) is_abundant = .true.
    end function is_abundant

    !> Sum of proper divisors.
    pure integer(i32) function spd(n)
        integer(i32), intent(in) :: n
        integer(i32) :: i

        spd = 1
        do i = 2, int(sqrt(real(n, sp)))
            if (mod(n, i) == 0) then
                if (n/i == i) then
                    spd = spd + i
                else
                    spd = spd + i + n/i
                end if
            end if
        end do
    end function spd

end submodule euler_problem_0023_m

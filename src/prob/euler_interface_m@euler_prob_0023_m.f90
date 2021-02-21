submodule(euler_interface_m) euler_prob_0023_m
    implicit none

contains

    module character(len=20) function euler0023()
        write (euler0023, "(i20)") ans()
    end function euler0023

    function ans() result(ret)
        integer, allocatable :: abundant(:)
        integer, parameter :: min_ = 12, max_ = 28123
        logical :: can_be_written(max_)
        integer :: i, j, ret

        do i = min_, max_
            if (is_abundant(i)) call append(abundant, i)
        end do

        can_be_written = .false.
        do i = 1, size(abundant)
            do j = i, size(abundant)
                associate(x => abundant(i) + abundant(j))
                    if (x <= max_) can_be_written(x) = .true.
                end associate
            end do
        end do
        ret = sum(pack([(i, i = 1, max_)], .not. can_be_written))
    end function ans

    function sum_of_proper_divisors(val) result(ret)
        integer, intent(in) :: val
        integer :: ret, i

        ret = 1
        do i = 2, val/2 + 1
            if (mod(val, i) == 0) then
                ret = ret + i
            end if
        end do
    end function sum_of_proper_divisors

    function is_abundant(val) result(ret)
        integer, intent(in) :: val
        logical :: ret

        ret = .false.
        if (sum_of_proper_divisors(val) > val) ret = .true.
    end function is_abundant

end submodule euler_prob_0023_m

submodule(interface_m) euler_problem_0044_m
    implicit none

contains

    module character(len=20) function euler0044()
        write (euler0044, "(i20)") answer()
    end function euler0044

    pure integer(i64) function answer()
        integer(i64), parameter :: dims = 3000
        integer(i64) :: pentagonals(dims)
        integer(i64) :: i, j, min_

        i = 1; j = 1
        do while (i <= dims)
            if (is_pentagonal(j)) then
                pentagonals(i) = j
                i = i + 1
            end if
            j = j + 1
        end do

        min_ = huge(0_i64)
        do i = 1, dims
            do j = i + 1, dims
                associate (pm => pentagonals(i), pn => pentagonals(j))
                    if (is_pentagonal_pair(pm, pn)) min_ = min(min_, pn - pm)
                end associate
            end do
        end do
        answer = min_
    end function answer

    pure logical function is_pentagonal_pair(pj, pk)
        integer(i64), intent(in) :: pj, pk

        associate (s => pj + pk, d => pk - pj)
            if (is_pentagonal(s) .and. is_pentagonal(d)) then
                is_pentagonal_pair = .true.
            else
                is_pentagonal_pair = .false.
            end if
        end associate
    end function is_pentagonal_pair

    elemental logical function is_pentagonal(p)
        integer(i64), intent(in) :: p

        associate (x => sqrt(24.*real(p) + 1.))
            if (is_integer(x) .and. mod(int(x, i64), 6_i64) == 5_i64) then
                is_pentagonal = .true.
            else
                is_pentagonal = .false.
            end if
        end associate
    end function is_pentagonal

    elemental logical function is_integer(n)
        real(sp), intent(in) :: n

        if (n - floor(n) <= tiny_sp) then
            is_integer = .true.
        else
            is_integer = .false.
        end if
    end function is_integer

end submodule euler_problem_0044_m

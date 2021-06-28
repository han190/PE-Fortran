submodule(euler_interface_m) euler_prob_0042_m
    implicit none

contains

    module character(len=20) function euler0042()
        write (euler0042, "(i20)") answer()
    end function euler0042

    integer function answer()
        use euler_data_m, only: get_euler_data_0042
        implicit none

        integer :: i, j
        integer, parameter :: max_score = 26*20
        logical :: is_tri_num(max_score)
        character(len=:), allocatable :: names(:)

        call tri_num(max_score, is_tri_num)
        call get_euler_data_0042(names)

        j = 0

        do i = 1, size(names)
            if (is_tri_num(score_of_word(names(i)))) then
                j = j + 1
            end if
        end do

        answer = j
    end function answer

    integer function score_of_word(str)
        character(len=*), intent(in) :: str
        integer :: j, isum

        isum = 0

        do j = 1, len_trim(str)
            isum = isum + iachar(str(j:j)) - 64
        end do

        score_of_word = isum
    end function score_of_word

    subroutine tri_num(n, is_tri_num)
        integer, intent(in) :: n
        logical, intent(out) :: is_tri_num(n)
        integer :: i

        is_tri_num = .false.

        i = 1
        do while (i*(i + 1)/2 <= n)
            is_tri_num(i*(i + 1)/2) = .true.
            i = i + 1
        end do
    end subroutine tri_num
end submodule euler_prob_0042_m

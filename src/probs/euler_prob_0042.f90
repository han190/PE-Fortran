submodule(euler_interface_m) euler_prob_0042_m
    implicit none

contains

    module character(len=20) function euler0042()
        write (euler0042, "(i20)") ans(2000)
    end function euler0042

    integer function ans(n)
        integer, intent(in) :: n
        character(len=500) :: cwd, filename, names(n)
        integer :: stat, i, j, e
        ! max_score is the score for name zzzzzzzzzzzzzzzzzzzzz
        integer, parameter :: max_score = 26 * 20
        logical :: is_tri_num(max_score)

        call tri_num(max_score, is_tri_num)

        names = 'n/a'
        call getcwd(cwd); e = len( trim(cwd) )
        filename = cwd(1:e - 3)//"/data/euler0042.txt"
        open(unit = 42, file = filename, status = "old", action = "read")
        read (42, *, iostat=stat) names(1:n)

        i = 1
        j = 0

        do while ( names(i) /= 'n/a' )
            if ( is_tri_num( score_of_word( names(i) ) ) ) then
                j = j + 1
            end if
            i = i + 1
        end do

        ans = j
    end function ans

    integer function score_of_word(str)
        character(*), intent(in) :: str
        integer :: j, isum

        isum = 0

        do j = 1, len_trim(str)
            isum = isum + iachar( str(j:j) ) - 64
        end do

        score_of_word = isum
    end function score_of_word

    subroutine tri_num(n, is_tri_num)
        integer, intent(in) :: n
        logical, intent(out) :: is_tri_num(n)
        integer :: i

        is_tri_num = .false.

        i = 1
        do while ( i * (i + 1) / 2 <= n )
            is_tri_num( i * (i + 1) / 2 ) = .true.
            i = i + 1
        end do
    end subroutine tri_num
end submodule euler_prob_0042_m

submodule(euler_interface_m) euler_prob_0022_m
    implicit none

contains

    module character(len=20) function euler0022()
        write (euler0022, "(i20)") ans()
    end function euler0022

    integer function ans()
        use euler_lexical_sort_m, only: lexical_sort
        implicit none
        integer :: i, j, istat, tmp
        character(len=:), allocatable :: names(:)

        open (unit=22, file="euler0022.txt", status="old", action="read")
        allocate(character(len=20) :: names(6000))
        names = "n/a"
        read (22, *, iostat=istat) names

        i = count(names /= "n/a")
        call lexical_sort(names(1:i))
        tmp = 0
        do j = 1, i
            tmp = tmp + j*score_of_letters(names(j))
        end do

        ans = tmp
    end function ans

    integer function score_of_letters(str)
        character(*), intent(in) :: str
        integer :: j, s

        s = 0
        do j = 1, len_trim(str)
            s = s + iachar(str(j:j)) - 64
        end do
        score_of_letters = s
    end function score_of_letters

end submodule euler_prob_0022_m

submodule(euler_interface_m) euler_prob_0022_m
    implicit none 

contains 

    module character(len=20) function euler0022()
        write (euler0022, "(i20)") ans()
    end function euler0022

    integer(int64) function ans()
        use euler_lexical_sort_m, only: lexical_sort 
        implicit none 
        character(len=500) :: cwd, filename 
        integer(int64) :: e, i, j, stat, tmp 
        character(len=20) :: arr_of_names(6000)

        call getcwd(cwd); e = len( trim(cwd) )
        filename = cwd(1:e - 3)//"/dat/euler0022.txt"
        open( unit = 22, file = filename, status = "old", action = "read" )
        arr_of_names = "n/a"
        read ( 22, *, iostat = stat ) arr_of_names(1:6000)

        i = 1_int64
        do while ( arr_of_names(i) /= "n/a" )
            i = i + 1_int64
        end do
        i = i - 1_int64

        call lexical_sort( arr_of_names(1:i) )
        tmp = 0_int64 
        do j = 1_int64, i
            tmp = tmp + j * score_of_letters( arr_of_names(j) )
        end do  

        ans = tmp 
    end function ans 

    integer(int64) function score_of_letters(str)
        character(*), intent(in) :: str
        integer(int64) :: j, s

        s = 0_int64
        do j = 1_int64, len_trim(str)
            s = s + iachar( str(j:j) ) - 64_int64
        end do
        score_of_letters = s
    end function score_of_letters

end submodule euler_prob_0022_m
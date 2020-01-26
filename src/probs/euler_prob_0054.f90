submodule(euler_interface_m) euler_problem_0054
    implicit none 

contains

    module character(len=20) function euler0054()
        write (euler0054, "(i20)") ans()
    end function euler0054

    integer function ans()
        use euler_texas_holdem_m
        implicit none 

        type(texas_holdem_t) :: cards
        character(2) :: char_arr(10)
        character(len=500) :: cwd, filename
        integer :: istat, i
        integer :: e 

        call getcwd(cwd); e = len( trim(cwd) )
        filename = cwd(1:e - 3)//"/dat/euler0054.txt"
        open(unit = 54, file = filename, status = "old", action = "read")

        i = 0 

        do
            read (54, *, iostat = istat) char_arr(:)
            if (istat /= 0) exit 

            cards = char_arr(:)
            if ( cards%compare() ) i = i + 1
        end do 

        close(54)
        ans = i
    end function ans 

end submodule euler_problem_0054
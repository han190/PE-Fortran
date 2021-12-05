submodule(interface_m) euler_problem_0022_m
    implicit none

contains

    module pure character(len=20) function euler0022()
        write (euler0022, "(i20)") answer()
    end function euler0022

    pure integer(i32) function answer()
        use data_m, only: get_euler_data_0022
        implicit none

        integer(i32) :: i
        character(len=:), allocatable :: names(:)

        call get_euler_data_0022(names)
        call lexical_sort(names)
        answer = sum([(i*score_of_letters(names(i)), i=1, size(names))])
    end function answer

    pure integer function score_of_letters(str)
        character(*), intent(in) :: str
        integer(i32) :: i

        score_of_letters = sum([(iachar(str(i:i)) - 64, i=1, len_trim(str))])
    end function score_of_letters

    pure subroutine lexical_sort(a)
        character(len=*), intent(inout) :: a(:)

        call quick_sort(a, 1, size(a))
    end subroutine lexical_sort

    pure recursive subroutine quick_sort(a, low, high)
        character(len=*), intent(inout) :: a(:)
        integer(i32), intent(in) :: low, high
        integer(i32) :: p

        if (low < high) then
            call partition(a, low, high, p)
            call quick_sort(a, low, p - 1)
            call quick_sort(a, p + 1, high)
        end if
    end subroutine quick_sort

    pure subroutine partition(a, low, high, i)
        character(len=*), intent(inout) :: a(:)
        integer(i32), intent(in) :: low, high
        integer(i32), intent(inout) :: i
        character(len=:), allocatable :: pivot
        integer(i32) :: j

        allocate (character(len=len(a(high))) :: pivot)
        pivot = a(high)
        i = low
        do j = low, high
            if (a(j) < pivot) then
                call swap(a(i), a(j))
                i = i + 1
            end if
        end do
        call swap(a(i), a(high))
    end subroutine partition

    pure subroutine swap(a, b)
        character(len=*), intent(inout) :: a
        character(len=len(a)), intent(inout) :: b
        character(len=len(a)) :: temp

        temp = a; a = b; b = temp
    end subroutine swap

end submodule euler_problem_0022_m

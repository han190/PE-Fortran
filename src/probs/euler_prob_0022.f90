submodule(euler_interface_m) euler_prob_0022_m
    implicit none

contains

    module character(len=20) function euler0022()
        write (euler0022, "(i20)") ans()
    end function euler0022

    integer function ans()
        integer :: i, j, istat, tmp
        character(len=:), allocatable :: names(:)

        open (unit=22, file="euler0022.txt", status="old", action="read")
        allocate (character(len=20) :: names(6000))
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

    subroutine lexical_sort(a)
        character(len=*), intent(inout) :: a(:)

        call quick_sort(a, 1, size(a))
    end subroutine lexical_sort

    recursive subroutine quick_sort(a, low, high)
        character(len=*), intent(inout) :: a(:)
        integer, intent(in) :: low, high
        integer :: p

        if (low < high) then
            p = partition(a, low, high)
            call quick_sort(a, low, p - 1)
            call quick_sort(a, p + 1, high)
        end if
    end subroutine quick_sort

    function partition(a, low, high) result(i)
        character(len=*), intent(inout) :: a(:)
        integer, intent(in) :: low, high
        character(len=:), allocatable :: pivot
        integer :: i, j

        allocate (character(len=len(a(high))) :: pivot)
        pivot = a(high)
        i = low
        do j = low, high
            if (a(j) < pivot) then
                call swap_chr(a(i), a(j))
                i = i + 1
            end if
        end do
        call swap_chr(a(i), a(high))
    end function partition

    subroutine swap_chr(a, b)
        character(len=*), intent(inout) :: a, b
        character(len=len(a)) :: tmp

        tmp = a
        a = b
        b = tmp
    end subroutine swap_chr

end submodule euler_prob_0022_m

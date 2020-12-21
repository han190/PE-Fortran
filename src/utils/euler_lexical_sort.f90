module euler_lexical_sort_m

    implicit none
    private
    public :: lexical_sort

contains

    subroutine swap(a, b)
        character(len=*), intent(inout) :: a, b
        character(len=len(a)) :: tmp

        tmp = a
        a = b
        b = tmp
    end subroutine swap

    function partition(a, low, high) result(i)
        character(len=*), intent(inout) :: a(:)
        integer, intent(in) :: low, high
        character(len=:), allocatable :: pivot
        integer :: i, j

        pivot = a(high)
        i = low
        do j = low, high
            if (a(j) < pivot) then
                call swap(a(i), a(j))
                i = i + 1
            end if
        end do
        call swap(a(i), a(high))
    end function partition

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

    subroutine lexical_sort(a)
        character(len=*), intent(inout) :: a(:)

        call quick_sort(a, 1, size(a))
    end subroutine lexical_sort

end module euler_lexical_sort_m

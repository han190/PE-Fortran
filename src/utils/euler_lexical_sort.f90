module euler_lexical_sort_m

    use euler_utils_m, only: swap
    implicit none
    private

    integer, allocatable :: index_arr(:)
    logical :: case_sensitive
    public :: lexical_sort

contains

    subroutine lexical_sort(str_arr, case_insensitive)
        character(len=*), intent(inout) :: str_arr(:)
        logical, intent(in), optional :: case_insensitive
        integer :: low, high, ios, k

        if (present(case_insensitive)) then
            case_sensitive = .not. case_insensitive
        else
            case_sensitive = .true.
        end if

        low = 1
        high = size(str_arr)
        allocate (index_arr(high), stat=ios)

        if (ios /= 0) error stop "Error allocating index_arr"
        !index_arr = (/(k, k=low, high)/)

        do k = low, high
            index_arr(k) = k
        end do

        call quick_sort(str_arr, low, high)

        str_arr = str_arr(index_arr)

        deallocate (index_arr, stat=ios)
        if (ios /= 0) error stop "Error allocating index_arr"
    end subroutine lexical_sort

    recursive subroutine quick_sort(str_arr, low, high)
        character(len=*), intent(inout) :: str_arr(:)
        integer, intent(in) :: low, high
        integer :: pivot_loc

        if (low < high) then
            call partition(str_arr, low, high, pivot_loc)
            call quick_sort(str_arr, low, pivot_loc - 1)
            call quick_sort(str_arr, pivot_loc + 1, high)
        end if
    end subroutine quick_sort

    subroutine partition(str_arr, low, high, pivot_loc)
        character(len=*), intent(inout) :: str_arr(:)
        integer, intent(in) :: low, high
        integer, intent(out) :: pivot_loc
        integer :: k, lastsmall

        call swap( &
            index_arr(low), &
            index_arr((low + high)/2) &
            )
        lastsmall = low

        do k = low + 1, high
            if ( &
                string_comp( &
                str_arr(index_arr(k)), &
                str_arr(index_arr(low)) &
                ) &
                ) then
                lastsmall = lastsmall + 1
                call swap( &
                    index_arr(lastsmall), &
                    index_arr(k) &
                    )
            end if
        end do

        call swap( &
            index_arr(low), &
            index_arr(lastsmall) &
            )
        pivot_loc = lastsmall
    end subroutine partition

    function string_comp(p, q) result(lexical_less)
        character(len=*), intent(in) :: p, q
        logical :: lexical_less
        integer :: kq, k

        if (case_sensitive) then
            lexical_less = p < q
        else
            kq = 1
            do k = 1, max(LEN_trim(p), LEN_trim(q))
                if (upper_case(p(k:k)) == upper_case(q(k:k))) then
                    cycle
                else
                    kq = k
                    exit
                end if
            end do
            lexical_less = upper_case(p(kq:kq)) < upper_case(q(kq:kq))
        end if
    end function string_comp

    function upper_case(letter) result(L)
        character(len=*), intent(in) :: letter
        character(len=1) :: L
        character(len=26), parameter :: &
            Lower = "abcdefghijklmnopqrstuvwxyz", &
            Upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        integer :: k

        k = index(Lower, letter)

        if (k > 0) then
            L = Upper(k:k)
        else
            L = letter
        end if
    end function upper_case

end module euler_lexical_sort_m

submodule(euler_interface_m) euler_prob_0059_m
    implicit none

contains

    module character(len=20) function euler0059()
        write (euler0059, "(i20)") ans()
    end function euler0059

    integer function ans()
        integer, allocatable :: encrypted(:), decrypted(:)
        integer, parameter :: n = 26, k = 3
        integer :: letters(n), idx(k), i, idx2(6, 3)
        logical :: next_permutation_avail

        next_permutation_avail = .true.
        encrypted = read_file("euler0059.txt")
        letters = [(i, i = 97, 122)]
        allocate(decrypted(size(encrypted)))
        idx = [1, 2, 3]
        idx2(1, :) = [1, 2, 3]
        idx2(2, :) = [1, 3, 2]
        idx2(3, :) = [2, 1, 3]
        idx2(4, :) = [2, 3, 1]
        idx2(5, :) = [3, 1, 2]
        idx2(6, :) = [3, 2, 1]

        outer: do while (next_permutation_avail)    
            inner: do i = 1, size(idx2(:, 1))
                associate (ii => idx(idx2(i, :)))
                    call decrypt(encrypted, letters(ii), decrypted)
                    if (is_english(decrypted)) then
                        ans = sum(decrypted)
                        return
                    end if
                end associate
            end do inner
            next_permutation_avail = next_combination(k, n, idx)
        end do outer
    end function ans

    subroutine remove_trailing_zeros(arr)
        integer, allocatable, intent(inout) :: arr(:)
        integer, allocatable :: tmp(:)
        integer :: i

        do i = 1, size(arr)
            if (all(arr(i:) == 0)) exit
        end do
        allocate (tmp(size(arr(1:i - 1))))
        tmp = arr(1:i - 1)
        call move_alloc(tmp, arr)
    end subroutine remove_trailing_zeros

    function read_file(filename) result(arr)
        character(len=*), intent(in) :: filename
        integer, allocatable :: arr(:)
        integer :: iunit, istat
        integer, parameter :: sufficiently_large_number = 5000

        iunit = 59
        open(unit=iunit, file=filename, iostat=istat, status="old")
        allocate (arr(sufficiently_large_number))
        arr = 0
        read(iunit, *, iostat=istat) arr
        call remove_trailing_zeros(arr)
    end function read_file

    subroutine decrypt(encrypted, key, decrypted)
        integer, intent(in) :: encrypted(:), key(:)
        integer, intent(out) :: decrypted(:)
        integer :: i, k

        do i = 1, size(encrypted)
            k = mod(i, 3)
            if (k == 0) k = 3
            decrypted(i) = ieor(encrypted(i), key(k))
        end do
    end subroutine decrypt

    function is_english(arr) result(ret)
        integer, intent(in) :: arr(:)
        logical :: ret
        character(len=1) :: check_(8)
        integer :: i, knt(8)

        check_ = ["e", "t", "a", "o", "i", "n", "s", "h"]
        do i = 1, size(check_)
            knt(i) = count(arr == iachar(check_(i))) 
        end do

        ret = .false.
        if (sum(knt)/real(size(arr))*100. > 45. .and. &
            maxloc(knt, dim=1) == 1) ret = .true.
    end function is_english

    function next_combination(k, n, idx) result(ret)
        integer, intent(in) :: k, n
        integer, intent(inout) :: idx(k)
        logical :: ret, carr(k)
        integer :: i, x, end_arr(k)

        end_arr = [(i, i=n - k + 1, n)]
        ret = .true.
        if (all(idx == end_arr)) then
            ret = .false.
            return
        end if

        carr = .true.
        label_carry: do i = k, 1, -1
            if (idx(i) == n - k + i) carr(i) = .false.
        end do label_carry

        if (all(carr .eqv. .true.)) then
            idx(k) = idx(k) + 1
        else
            x = findloc(carr, value=.false., dim=1) - 1
            idx(x:k) = [(idx(x) + i, i=1, k - x + 1)]
        end if
    end function next_combination

end submodule euler_prob_0059_m

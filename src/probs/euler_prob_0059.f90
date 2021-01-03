submodule(euler_interface_m) euler_prob_0059_m
    implicit none

contains

    module character(len=20) function euler0059()
        write (euler0059, "(i20)") ans()
    end function euler0059

    integer function ans()
        integer, allocatable :: encrypted(:), decrypted(:)
        integer, parameter :: n = 26, k = 3
        integer :: letters(n), idx(k), i, idx3(k), idx2(2*k, k)
        logical :: next_permutation_avail

        next_permutation_avail = .true.
        encrypted = read_file("euler0059.txt")
        letters = [(i, i=97, 122)]
        allocate (decrypted(size(encrypted)))
        idx = [1, 2, 3]
        idx2(1, :) = [1, 2, 3]
        idx2(2, :) = [1, 3, 2]
        idx2(3, :) = [2, 1, 3]
        idx2(4, :) = [2, 3, 1]
        idx2(5, :) = [3, 1, 2]
        idx2(6, :) = [3, 2, 1]

        outer: do while (next_permutation_avail)
            inner: do i = 1, size(idx2(:, 1))
                idx3(:) = idx(idx2(i, :))
                if (is_english(decrypted, letters(idx3))) then
                    ans = sum(decrypted)
                    return
                end if
            end do inner
            next_permutation_avail = next_permutation(k, n, idx)
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
        open (unit=iunit, file=filename, iostat=istat, status="old")
        allocate (arr(sufficiently_large_number))
        arr = 0
        read (iunit, *, iostat=istat) arr
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

    function is_english(encrypted, key) result(ret)
        integer, intent(in) :: encrypted(:), key(:)
        logical :: ret
        character(len=1) :: check_(8)
        integer :: i, knt(8), decrypted(size(encrypted))

        call decrypt(encrypted, key, decrypted)
        check_ = ["e", "t", "a", "o", "i", "n", "s", "h"]
        do i = 1, size(check_)
            knt(i) = count(decrypted == iachar(check_(i)))
        end do

        ret = .false.
        if (sum(knt)/real(size(decrypted))*100. > 45. .and. &
            maxloc(knt, dim=1) == 1) ret = .true.
    end function is_english

end submodule euler_prob_0059_m
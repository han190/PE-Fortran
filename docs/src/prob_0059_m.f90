submodule(euler_interface_m) euler_prob_0059_m
    implicit none

contains

    module character(len=20) function euler0059()
        write (euler0059, "(i20)") answer()
    end function euler0059

    function answer() result(ret)
        use euler_data_m, only: get_euler_data_0059
        implicit none

        integer :: ret
        integer, allocatable :: encrypted(:), decrypted(:)
        integer, parameter :: n = 26, k = 3
        integer :: letters(n), idx(k), idx2(2*k, k), key(k), i, j
        logical :: next_permutation_avail
        integer :: large_number

        next_permutation_avail = .true.

        call get_euler_data_0059(encrypted)
        large_number = size(encrypted)

        letters = [(i, i=97, 122)]
        allocate (decrypted(size(encrypted)))
        idx = [1, 2, 3]
        idx2(1, :) = [1, 2, 3]
        idx2(2, :) = [1, 3, 2]
        idx2(3, :) = [2, 1, 3]
        idx2(4, :) = [2, 3, 1]
        idx2(5, :) = [3, 1, 2]
        idx2(6, :) = [3, 2, 1]

        ret = 0
        outer: do while (next_permutation_avail)
            inner: do i = 1, size(idx2(:, 1))
                innermost: do j = 1, 3
                    key(j) = letters(idx(idx2(i, j)))
                end do innermost
                call decrypt(encrypted, key, decrypted)
                if (is_english(decrypted)) then
                    ret = sum(decrypted)
                    return
                end if
            end do inner
            call next_permutation(k, n, idx, next_permutation_avail)
        end do outer
    end function answer

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

    function is_english(decrypted) result(ret)
        integer, intent(in) :: decrypted(:)
        logical :: ret
        character(len=1) :: check_(8)
        integer :: i, knt(8)

        check_ = ["e", "t", "a", "o", "i", "n", "s", "h"]
        do i = 1, size(check_)
            knt(i) = count(decrypted == iachar(check_(i)))
        end do

        ret = .false.
        if (sum(knt)/real(size(decrypted))*100. > 45. .and. &
            maxloc(knt, dim=1) == 1) ret = .true.
    end function is_english

end submodule euler_prob_0059_m

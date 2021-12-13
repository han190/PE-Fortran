submodule(interface_m) euler_prob_0059_m
    implicit none

contains

    module character(len=20) function euler0059()
        write (euler0059, "(i20)") answer()
    end function euler0059

    integer(i32) function answer()
        integer(i32), parameter :: n = 26, k = 3
        integer(i32), allocatable :: encrypted(:), decrypted(:)
        integer(i32) :: letters(n), idx(k), idx2(k), i, j
        logical :: available, available2
        integer(i32) :: iunit, istat

        encrypted = [(-1, i=1, 2000)]
        open (newunit=iunit, file="data_0059.txt", &
              action="read", status="old")
        read (iunit, *, iostat=istat) encrypted
        close (iunit)
        encrypted = pack(encrypted, encrypted /= -1)

        letters = [(i, i=97, 122)]
        allocate (decrypted(size(encrypted)))

        idx = [1, 2, 3]
        available = .true.
        outer: do while (available)

            idx2 = [1, 2, 3]
            available2 = .true.
            inner: do while (available2)
                call decrypt(encrypted, letters(idx(idx2)), decrypted)
                if (is_english(decrypted)) exit outer
                call permute(3, 3, idx2, available2)
            end do inner

            call permute(k, n, idx, available)
        end do outer
        answer = sum(decrypted)
    end function answer

    pure subroutine decrypt(encrypted, key, decrypted)
        integer, intent(in) :: encrypted(:), key(:)
        integer, intent(out) :: decrypted(:)
        integer :: i, k

        do i = 1, size(encrypted)
            k = mod(i, 3)
            if (k == 0) k = 3
            decrypted(i) = ieor(encrypted(i), key(k))
        end do
    end subroutine decrypt

    pure function is_english(decrypted) result(ret)
        integer, intent(in) :: decrypted(:)
        logical :: ret
        character(len=1) :: check_(8)
        integer :: i, knt(8)
        logical :: english_features

        check_ = ["e", "t", "a", "o", "i", "n", "s", "h"]
        knt = [(count(decrypted == iachar(check_(i))), i=1, size(check_))]
        ret = .false.
        english_features = sum(knt)/real(size(decrypted))*100. > 45.
        if (english_features .and. maxloc(knt, dim=1) == 1) ret = .true.
    end function is_english

end submodule euler_prob_0059_m

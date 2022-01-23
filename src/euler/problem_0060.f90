submodule(interface_m) euler_problem_0060_m
    implicit none

contains

    module character(len=20) function euler0060()
        write (euler0060, "(i20)") answer()
    end function euler0060

    pure integer(i64) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i64), parameter :: n(2) = [5000000, 1060]
        integer(i64) :: i, k, flags(2)
        logical, allocatable :: is_prime(:), concs(:, :)
        integer(i64), allocatable :: primes(:), idx(:)
        logical :: avail, checked, succeed

        allocate (is_prime(n(1)))
        call Sieve_of_Eratosthenes(n(1), is_prime)
        associate (upper_ => (count(is_prime)))
            primes = pack([(i, i=1, upper_)], [(is_prime(i), i=1, upper_)])
        end associate

        k = 5
        idx = [2, 3, 4, 5, 6]
        avail = .true.
        flags = [1, 2]

        allocate (concs(n(2), n(2)))
        concs = .true.

        do while (avail)
            call checked_before(idx, concs, flags, checked)
            if (.not. checked) then
                call is_prime_pair(primes(idx), is_prime, flags, succeed)
                if (succeed) exit
                call label(flags, idx, concs)
            end if
            call next_combination(k, n(2), flags, idx, avail)
        end do

        answer = sum(primes(idx))
    end function answer

    pure subroutine is_prime_pair(pair, is_prime_, flag, succeed)
        use prime_m, only: is_prime
        implicit none

        integer(i64), intent(in) :: pair(:)
        logical, intent(in) :: is_prime_(:)
        integer(i64), intent(out) :: flag(2)
        logical, intent(out) :: succeed
        integer(i64) :: idx(2), arr(2)
        integer(i64) :: i, size_
        logical :: avail

        idx = [1, 2]
        avail = .true.
        flag = 0
        succeed = .true.
        size_ = size(pair, kind=i64)

        do while (avail)
            arr = [concat(pair(idx)), concat(pair(idx(2:1:-1)))]
            do i = 1, 2
                succeed = merge(is_prime_(arr(i)), is_prime(arr(i)), &
                                arr(i) <= size(is_prime_))

                if (.not. succeed) then
                    flag = idx
                    return
                end if
            end do

            call permute(size_, idx, avail)
        end do
    end subroutine is_prime_pair

    pure subroutine checked_before(idx, concs, flags, ret)
        integer(i64), intent(in) :: idx(:)
        logical, intent(in) :: concs(:, :)
        integer(i64), intent(inout) :: flags(2)
        logical, intent(out) :: ret
        logical :: avail
        integer(i64) :: i(2), size_

        i = [1, 2]
        ret = .false.
        avail = .true.
        size_ = size(idx, kind=i64)

        do while (avail)
            associate (x => idx(i), s => size(concs(1, :)))
                if (all(x < s) .and. .not. concs(x(1), x(2))) then
                    ret = .true.; flags = i; return
                end if
            end associate
            call permute(size_, i, avail)
        end do
    end subroutine checked_before

    pure subroutine next_combination(k, n, flags, idx, ret)
        integer(i64), intent(in) :: k, n, flags(2)
        integer(i64), intent(inout) :: idx(:)
        logical, intent(out) :: ret
        integer(i64) :: i

        if (all(idx == [(i, i=n - k + 1, n)])) then
            ret = .false.; return
        end if

        ret = .true.
        associate (f => (max(flags(1), flags(2))))
            if (f == k .or. f == 0 .or. idx(size(idx)) == n) then
                call permute(n, idx, ret)
            else
                idx(f:k) = [(idx(f) + i, i=1, k - f + 1)]
            end if
        end associate
    end subroutine next_combination

    pure subroutine label(flags, idx, conc)
        integer(i64), intent(in) :: flags(2), idx(:)
        logical, intent(inout) :: conc(:, :)

        associate (x => (idx(flags(1))), y => (idx(flags(2))))
            if (all([x, y] >= size(conc(1, :)))) return
            if (conc(x, y)) then
                conc(x, y) = .false.; conc(y, x) = .false.
            end if
        end associate
    end subroutine label

    pure integer(i64) function concat(arr)
        integer(i64), intent(in) :: arr(:)

        concat = arr(1)*10**number_of_digits(arr(2)) + arr(2)
    end function concat

end submodule euler_problem_0060_m

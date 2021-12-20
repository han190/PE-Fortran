submodule(interface_m) euler_problem_0060_m
    implicit none

contains

    module character(len=20) function euler0060()
        write (euler0060, "(i20)") answer()
    end function euler0060

    integer(i64) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i64), parameter :: n(2) = [5000000, 1060]
        integer(i64) :: i, k, flags(2)
        logical, allocatable :: is_prime(:), concs(:, :)
        integer(i64), allocatable :: primes(:), idx(:)
        logical :: avail

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
            if (.not. checked_before(idx, concs, flags)) then
                if (is_prime_pair(primes(idx), is_prime, flags)) exit
                call label(flags, idx, concs)
            end if
            call next_combination(k, n(2), flags, idx, avail)
        end do

        answer = sum(primes(idx))
    end function answer

    function is_prime_pair(test_pair, isprime, flag) result(try_succeed)
        use prime_m, only: is_prime
        implicit none

        integer(i64), intent(in) :: test_pair(:)
        logical, allocatable, intent(in) :: isprime(:)
        integer(i64), intent(out) :: flag(2)
        integer(i64) :: k, n, idx(2), ab_arr(2), ba_arr(2)
        logical :: avail, try_succeed

        k = 2
        n = size(test_pair)

        try_succeed = .true.
        idx = [1, 2]
        avail = .true.
        flag = 0

        do while (avail)
            ab_arr = test_pair(idx)
            ba_arr = test_pair(idx(2:1:-1))
            associate (ab => concatenate(ab_arr), ba => concatenate(ba_arr))
                if (ab <= size(isprime)) then
                    if (.not. isprime(ab)) then
                        flag = idx
                        try_succeed = .false.
                        return
                    end if
                else
                    if (.not. is_prime(ab)) then
                        flag = idx
                        try_succeed = .false.
                        return
                    end if
                end if

                if (ba <= size(isprime)) then
                    if (.not. isprime(ba)) then
                        flag = idx(2:1:-1)
                        try_succeed = .false.
                        return
                    end if
                else
                    if (.not. is_prime(ba)) then
                        flag = idx(2:1:-1)
                        try_succeed = .false.
                        return
                    end if
                end if
                call permute(k, n, idx, avail)
            end associate
        end do
    end function

    function checked_before(test_idx, conc_is_prime, flags) result(ret)
        integer(i64), intent(in) :: test_idx(:)
        logical, intent(in) :: conc_is_prime(:, :)
        integer(i64), intent(inout) :: flags(2)
        logical :: ret, next_avail
        integer(i64) :: k, n, idx(2)

        k = 2
        n = size(test_idx)
        idx = [1, 2]
        ret = .false.
        next_avail = .true.

        do while (next_avail)
            associate (x => test_idx(idx(1)), y => test_idx(idx(2)), &
                       s => size(conc_is_prime(1, :)))
                if (x < s .and. y < s) then
                    if (.not. conc_is_prime(x, y)) then
                        ret = .true.
                        flags = idx
                        return
                    end if
                end if
            end associate
            call permute(k, n, idx, next_avail)
        end do
    end function checked_before

    pure subroutine next_combination(k, n, flags, idx, ret)
        integer(i64), intent(in) :: k, n, flags(2)
        integer(i64), intent(inout) :: idx(:)
        logical, intent(out) :: ret
        integer(i64) :: i, end_arr(k), flag

        flag = max(flags(1), flags(2))
        if (flag == k .or. flag == 0_i64) then
            call permute(k, n, idx, ret)
            return
        end if

        end_arr = [(i, i=n - k + 1, n)]
        ret = .true.
        if (all(idx == end_arr)) then
            ret = .false.
            return
        end if

        if (idx(size(idx)) == n) then
            call permute(k, n, idx, ret)
        else
            idx(flag:k) = [(idx(flag) + i, i=1, k - flag + 1)]
        end if
    end subroutine next_combination

    pure subroutine label(flags, idx, conc)
        integer(i64), intent(in) :: flags(2), idx(:)
        logical, intent(inout) :: conc(:, :)

        associate (x => (idx(flags(1))), y => (idx(flags(2))))
            if (x >= size(conc(1, :)) .or. &
                y >= size(conc(1, :))) return
            if (conc(x, y)) then
                conc(x, y) = .false.
                conc(y, x) = .false.
            end if
        end associate
    end subroutine label

    pure integer(i64) function concatenate(arr)
        integer(i64), intent(in) :: arr(:)

        concatenate = arr(1)*10**number_of_digits(arr(2)) + arr(2)
    end function concatenate

end submodule euler_problem_0060_m

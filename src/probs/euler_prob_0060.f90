submodule(euler_interface_m) euler_prob_0060_m
    implicit none

contains

    module character(len=20) function euler0060()
        integer(int64) :: input_arr(2)

        input_arr = [5000000_int64, 1060_int64]
        write (euler0060, "(i20)") ans(input_arr)
    end function euler0060

    integer(int64) function ans(n)
        use euler_primes_m, only: sieve_of_Eratosthenes
        implicit none

        integer(int64), intent(in) :: n(2)
        integer(int64) :: i, k, flags(2)
        logical, allocatable :: isprime(:), conc_is_prime(:, :)
        integer(int64), allocatable :: primes(:), test_pair(:), idx(:)
        logical :: prime_pair_found, next_permutation_avail, checked

        call sieve_of_Eratosthenes(n(1), isprime)
        allocate (primes(count(isprime)))
        k = 1
        do i = 0, size(isprime) - 1
            if (isprime(i)) then
                primes(k) = i
                k = k + 1
            end if
        end do

        k = 5
        allocate (idx(k))
        idx = [2, 3, 4, 5, 6]
        prime_pair_found = .false.
        next_permutation_avail = .true.
        flags = [1, 2]

        allocate (conc_is_prime(n(2), n(2)))
        conc_is_prime = .true.

        do while (next_permutation_avail)
            checked = checked_before(idx, conc_is_prime, flags)
            if (checked) then
                next_permutation_avail = next_combination(k, n(2), flags, idx)
                cycle
            end if

            test_pair = primes(idx)
            prime_pair_found = is_prime_pair(test_pair, isprime, flags)
            if (prime_pair_found) exit
            call gen_label(flags, idx, conc_is_prime)
            next_permutation_avail = next_combination(k, n(2), flags, idx)
        end do

        ans = sum(primes(idx))
    end function

    function concatenate(arr) result(ab)
        integer(int64), intent(in) :: arr(:)
        integer(int64) :: ab, pow

        pow = digs_of_int(arr(2))
        ab = arr(1)*10**pow + arr(2)
    end function concatenate

    function checked_before(test_idx, conc_is_prime, flags) result(ret)
        integer(int64), intent(in) :: test_idx(:)
        logical, intent(in) :: conc_is_prime(:, :)
        integer(int64), intent(inout) :: flags(2)
        logical :: ret, next_avail
        integer(int64) :: k, n, idx(2)

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
            next_avail = next_permutation(k, n, idx)
        end do
    end function checked_before

    subroutine gen_label(flags, idx, conc_is_prime)
        integer(int64), intent(in) :: flags(2), idx(:)
        logical, intent(inout) :: conc_is_prime(:, :)

        associate (x => idx(flags(1)), y => idx(flags(2)), &
                   s => size(conc_is_prime(1, :)))
            if (x < s .and. y < s) then
                if (conc_is_prime(x, y)) then
                    conc_is_prime(x, y) = .false.
                    conc_is_prime(y, x) = .false.
                end if
            end if
        end associate
    end subroutine gen_label

    function next_combination(k, n, flags, idx) result(ret)
        integer(int64), intent(in) :: k, n, flags(2)
        integer(int64), intent(inout) :: idx(:)
        logical :: ret
        integer(int64) :: i, end_arr(k), flag

        flag = max(flags(1), flags(2))
        if (flag == k .or. flag == 0_int64) then
            ret = next_permutation(k, n, idx)
            return
        end if

        end_arr = [(i, i=n - k + 1, n)]
        ret = .true.
        if (all(idx == end_arr)) then
            ret = .false.
            return
        end if

        if (idx(size(idx)) == n) then
            ret = next_permutation(k, n, idx)
        else
            idx(flag:k) = [(idx(flag) + i, i=1, k - flag + 1)]
        end if
    end function next_combination

    function is_prime_pair(test_pair, isprime, flag) result(try_succeed)
        use euler_primes_m, only: is_prime
        implicit none

        integer(int64), intent(in) :: test_pair(:)
        logical, allocatable, intent(in) :: isprime(:)
        integer(int64), intent(out) :: flag(2)
        integer(int64) :: k, n, idx(2)
        logical :: avail, try_succeed

        k = 2
        n = size(test_pair)

        try_succeed = .true.
        idx = [1, 2]
        avail = .true.
        flag = 0

        do while (avail)
            associate (ab => concatenate(test_pair(idx)), &
                       ba => concatenate(test_pair(idx(2:1:-1))))
                if (ab <= size(isprime) .and. &
                    (.not. isprime(ab))) then
                        flag = idx
                        try_succeed = .false.
                        return
                else if (.not. is_prime(ab)) then
                        flag = idx
                        try_succeed = .false.
                        return
                end if

                if (ba <= size(isprime) .and. &
                    (.not. isprime(ba))) then
                        flag = idx(2:1:-1)
                        try_succeed = .false.
                        return
                else if (.not. is_prime(ba)) then
                        flag = idx(2:1:-1)
                        try_succeed = .false.
                        return
                end if
                avail = next_permutation(k, n, idx)
            end associate
        end do
    end function is_prime_pair

end submodule euler_prob_0060_m
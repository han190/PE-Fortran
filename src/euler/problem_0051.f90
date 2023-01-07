submodule(interface_module) euler_problem_0051_submodule
implicit none

contains

module character(len=20) function euler0051()
  write (euler0051, "(i20)") answer()
end function euler0051

elemental function answer() result(ret)
  use prime_module, only: Sieve_of_Eratosthenes
  implicit none

  integer(i32), parameter :: PRIME_START = 100000, PRIME_END = 999999
  integer(i32) :: i, j, s, idx, p_knt, c_knt, ret
  integer(i32), allocatable :: prime(:), arr(:)
  logical, allocatable :: is_prime(:)

  allocate (is_prime(PRIME_END))
  call Sieve_of_Eratosthenes(PRIME_END, is_prime)
  prime = pack([(i, i=1, PRIME_END)], is_prime)
  prime = pack(prime, prime > PRIME_START)

  outer: do idx = 1, size(prime)
    arr = to_array(prime(idx))
    associate (k => [(count(arr == i), i=0, 2)])
      j = findloc([(mod(k(i), 3) == 0, i=1, 3)], dim=1, value=.true.)
      if (j == 0) cycle outer
    end associate

    p_knt = 0; c_knt = 0
    associate (r => pack([(i, i=1, size(arr))], arr == arr(j)))
      s = merge(1, 0, any(r == 1))

      inner: do j = s, 9
        arr(r) = j
        if (is_prime(to_integer(arr))) then
          p_knt = p_knt + 1
        else
          c_knt = c_knt + 1
        end if
        if (p_knt >= 8) exit outer
        if (c_knt >= 3) exit inner
      end do inner
    end associate
  end do outer
  ret = prime(idx)
end function answer

end submodule euler_problem_0051_submodule


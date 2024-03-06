submodule(module_problem) submodule_euler0051
implicit none
contains

module subroutine euler0051(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: lower = 100000, upper = 999999
  integer(int64) :: i, j, k, s, idx, p_knt, c_knt
  integer(int64), allocatable :: array(:), primes(:)
  logical, allocatable :: check(:)

  check = sift(upper)
  primes = pack(check, lower, upper)

  k = 1
  do i = lower, upper
    if (check(i)) then
      primes(k) = i
      k = k + 1
    end if
  end do

  outer: do idx = 1, size(primes)
    array = to_array(primes(idx))
    associate (k => [(count(array == i), i=0, 2)])
      j = findloc([(mod(k(i), 3) == 0, i=1, 3)], dim=1, value=.true.)
      if (j == 0) cycle outer
    end associate

    p_knt = 0
    c_knt = 0
    associate (r => pack([(i, i=1, size(array))], array == array(j)))
      s = merge(1, 0, any(r == 1))

      inner: do j = s, 9
        array(r) = j
        if (check(to_integer(array))) then
          p_knt = p_knt + 1
        else
          c_knt = c_knt + 1
        end if
        if (p_knt >= 8) exit outer
        if (c_knt >= 3) exit inner
      end do inner
    end associate
  end do outer
  write (problem%answer, "(i20)") primes(idx)
end subroutine euler0051

end submodule submodule_euler0051

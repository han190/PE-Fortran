submodule(module_problem) submodule_euler0038
implicit none
contains

module subroutine euler0038(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64) :: i, j, tmp, sln

  sln = 0
  outer: do i = 2, 10000
    j = 1
    tmp = 0

    inner: do
      if (tmp /= 0) then
        if (num_digits(tmp) > 9) cycle outer
      end if

      if (is_pandigital(tmp)) then
        if (tmp > sln) sln = tmp
      end if

      tmp = i*j + tmp*10**num_digits(i*j)
      j = j + 1
    end do inner
  end do outer
  write (problem%answer, "(i20)") sln
end subroutine euler0038

end submodule submodule_euler0038

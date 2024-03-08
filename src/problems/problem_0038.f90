submodule(module_problem) submodule_euler0038
implicit none
contains

module subroutine euler0038(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
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
  write (answer, "(i20)") sln
end subroutine euler0038

end submodule submodule_euler0038

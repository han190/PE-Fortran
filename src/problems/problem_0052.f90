submodule(module_problem) submodule_euler0052
implicit none
contains

module subroutine euler0052(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64) :: i, j
  logical :: array(0:9), array_temp(0:9)

  i = 1
  outer: do
    call digits_in_use(i, array)
    inner: do j = 2, 6
      call digits_in_use(i*j, array_temp)
      if (all(array .eqv. array_temp)) then
        if (j == 6) exit outer
      else
        exit inner
      end if
    end do inner
    i = i + 1
  end do outer
  write (answer, "(i20)") i
end subroutine euler0052

pure subroutine digits_in_use(n, array)
  integer(int64), intent(in) :: n
  logical, intent(out) :: array(0:9)

  array = .false.
  array(to_array(n)) = .true.
end subroutine digits_in_use

end submodule submodule_euler0052

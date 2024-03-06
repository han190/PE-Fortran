submodule(module_problem) submodule_euler0099
implicit none
contains

module subroutine euler0099(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64) :: unit, iostat, current(2), largest(2), k, sln

  largest = [1, 1]
  open (newunit=unit, file=problem%file, &
    & iostat=iostat, delim="APOSTROPHE")
  k = 1
  do while (iostat == 0)
    read (unit, *, iostat=iostat) current
    if (compare(current, largest) == 1) then
      sln = k
      largest = current
    end if
    k = k + 1
  end do
  close (unit)
  write (problem%answer, "(i0)") sln
end subroutine euler0099

pure function compare(val0, val1) result(ret)
  integer(int64), intent(in) :: val0(2), val1(2)
  integer(int64) :: ret

  associate ( &
    & b0 => val0(1), p0 => val0(2), &
    & b1 => val1(1), p1 => val1(2))
    
    if ( &
      & (p0 > p1 .and. (b0 == b1 .or. b0 > b1)) .or. &
      & (p0 == p1 .and. b0 > b1)) then
      ret = 1
    else if ( &
      & (p0 < p1 .and. (b0 < b1 .or. b0 == b1)) .or. &
      & (p0 == p1 .and. b0 < b1)) then
      ret = -1
    else if (p0 == p1 .and. b0 == b1) then
      ret = 0
    else
      ret = merge(1, -1, &
        & real(p0)*log10(real(b0)) > real(p1)*log10(real(b1)))
    end if
  end associate
end function compare

end submodule submodule_euler0099
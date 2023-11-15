submodule(module_interface) submodule_euler0011
implicit none
contains

module subroutine euler0011(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32) :: array(20, 20), selected(4, 4)
  integer(int32) :: i, j, unit, sln

  open (newunit=unit, file=problem%file, status="old", action="read")
  read (unit, *) array
  close (unit)

  sln = 0
  do i = 1, 17
    do j = 1, 17
      selected = array(i:i + 3, j:j + 3)
      if (max_(selected) > sln) sln = max_(selected)
    end do
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0011

pure integer(int32) function max_(matrix)
  integer(int32), intent(in) :: matrix(4, 4)
  integer(int32) :: i

  associate ( &
    diagonal => product([(matrix(i, i), i=1, 4)]), &
    anti_diagonal => product([(matrix(i, 5 - i), i=1, 4)]), &
    top => product(matrix(1:4, 1)), &
    bottom => product(matrix(1:4, 4)), &
    left => product(matrix(1, 1:4)), &
    right => product(matrix(4, 1:4)))
    max_ = max(top, bottom, left, right, diagonal, anti_diagonal)
  end associate
end function max_

end submodule submodule_euler0011

submodule(interface_module) euler_problem_0011_submodule
implicit none

contains

module character(len=20) function euler0011()
  write (euler0011, "(i20)") answer()
end function euler0011

integer(i32) function answer()
  integer(i32) :: array(20, 20), selected(4, 4)
  integer(i32) :: i, j, iunit

  open (newunit=iunit, file=data_path//"/"//"data_0011.txt", &
        status="old", action="read")
  read (iunit, *) array
  close (iunit)

  answer = 0
  do i = 1, 17
    do j = 1, 17
      selected = array(i:i + 3, j:j + 3)
      if (max_(selected) > answer) answer = max_(selected)
    end do
  end do
end function answer

pure integer(i32) function max_(matrix)
  integer(i32), intent(in) :: matrix(4, 4)
  integer(i32) :: i

  associate (diagonal => product([(matrix(i, i), i=1, 4)]), &
             anti_diagonal => product([(matrix(i, 5 - i), i=1, 4)]), &
             top => product(matrix(1:4, 1)), &
             bottom => product(matrix(1:4, 4)), &
             left => product(matrix(1, 1:4)), &
             right => product(matrix(4, 1:4)))
    max_ = max(top, bottom, left, right, diagonal, anti_diagonal)
  end associate
end function max_

end submodule euler_problem_0011_submodule

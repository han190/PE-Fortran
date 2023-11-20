submodule(module_problem) submodule_euler0008
implicit none
contains

module subroutine euler0008(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64) :: array(1000), i, unit, sln

  open (newunit=unit, file=problem%file, status='old', action='read')
  do i = 1, 20
    read (unit, "(50(i1))") array((i - 1)*50 + 1:i*50)
  end do
  close (unit)

  sln = 0
  do i = 1, 988
    associate (prod => (product(array(i:i + 12))))
      if (prod > sln) sln = prod
    end associate
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0008

end submodule submodule_euler0008

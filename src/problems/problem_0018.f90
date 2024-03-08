submodule(module_problem) submodule_euler0018
implicit none
contains

module subroutine euler0018(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  integer(int64), parameter :: n = 15
  type(jagged_type), allocatable :: jagged(:)
  integer(int64) :: i, j, x, unit

  allocate (jagged(n))
  do i = 1, n
    allocate (jagged(i)%array(i))
  end do

  open (newunit=unit, file=file, status="old", action="read")
  do i = 1, n
    read (unit, *) jagged(i)%array
  end do
  close (unit)

  do j = n - 1, 1, -1
    do i = 1, j
      x = max(jagged(j + 1)%array(i), jagged(j + 1)%array(i + 1))
      jagged(j)%array(i) = x + jagged(j)%array(i)
    end do
  end do
  write (answer, "(i20)") jagged(1)%array(1)
end subroutine euler0018

end submodule submodule_euler0018
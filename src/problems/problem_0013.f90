submodule(module_problem) submodule_euler0013
implicit none
contains

module subroutine euler0013(answer, file)
  character(len=*), intent(out) :: answer
  character(len=*), intent(in) :: file
  
  ! integer(int64) :: sln(10)
  integer(int64) :: digs(50, 100), tmp(150), unit
  integer(int64) :: i, n

  open (newunit=unit, file=file, status="old", action="read")
  read (unit, "(50(i1))") digs
  close (unit)

  tmp = 0
  n = size(tmp)
  do i = 1, 100
    tmp(n - 49:n) = tmp(n - 49:n) + digs(:, i)
  end do

  call carry(tmp)
  do i = 1, n
    if (tmp(i) /= 0) exit
  end do
  write (answer, "(10(' '), 10(i1))") tmp(i:i + 9)
end subroutine euler0013

end submodule submodule_euler0013

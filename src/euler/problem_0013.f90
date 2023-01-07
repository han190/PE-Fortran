submodule(interface_module) euler_problem_0013_submodule
use multiprecision_module
implicit none

contains

module character(len=20) function euler0013()
  type(long_integer) :: value_
  value_ = answer()
  write (euler0013, "(10(' '), 10(i1))") value_%digit(1:10)
end function euler0013

type(long_integer) function answer()
  integer(i32) :: i, iunit
  type(long_integer) :: tmp

  open (newunit=iunit, file=data_path//"/"//"data_0013.txt", &
        status="old", action="read")

  tmp%sign = "+"
  allocate (tmp%digit(50))
  answer = 0

  do i = 1, 100
    read (iunit, "(50(i1))") tmp%digit
    answer = answer + tmp
  end do
  close (iunit)
end function answer

end submodule euler_problem_0013_submodule

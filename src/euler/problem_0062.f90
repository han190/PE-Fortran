submodule(interface_module) euler_problem_0062_submodule
implicit none

contains

module character(len=20) function euler0062()
  write (euler0062, "(i20)") answer()
end function euler0062

pure integer(i64) function answer()
  use multiprecision_module
  use stdlib_sorting, only: sort
  implicit none

  integer(i64) :: i, x
  integer(i64), allocatable :: count_(:), integer_array(:)
  character(len=20), allocatable :: list(:)
  integer(i64), parameter :: upper_ = 10000

  allocate (list(upper_), count_(upper_))
  list = ""; count_ = 1
  do i = 1, upper_
    integer_array = to_array(i**3)
    call sort(integer_array)
    write (list(i), "(*(i1))") integer_array
    x = findloc(list(:i - 1), value=list(i), dim=1)

    if (x /= 0) then
      count_(x) = count_(x) + 1
      if (count_(x) == 5) exit
    end if
  end do
  answer = x**3
end function answer

end submodule euler_problem_0062_submodule

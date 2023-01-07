submodule(interface_module) euler_problem_0014_submodule
implicit none

contains

module character(len=20) function euler0014()
  write (euler0014, "(i20)") answer()
end function euler0014

elemental integer(i64) function answer()
  integer(i64), allocatable :: array(:)
  integer(i64) :: tmp, length, longest_length, i
  integer(i64), parameter :: start = 5*10**5_i64, end = 10**6_i64

  allocate (array(end)); array = -1
  length = 0
  longest_length = 0
  answer = 0

  do i = 1, end
    tmp = i
    length = 0

    if (array(i) == -1) then
      do while (tmp /= 1)
        if (mod(tmp, 2_i64) == 0) then
          tmp = tmp/2
          length = length + 1
        else
          tmp = (tmp*3 + 1)/2
          length = length + 2
        end if
      end do
      array(i) = length
    else
      length = array(i)
    end if

    if (length > longest_length) then
      longest_length = length
      answer = i
    end if
  end do
end function answer

end submodule euler_problem_0014_submodule

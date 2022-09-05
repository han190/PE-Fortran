submodule(interface_module) euler_problem_0021_submodule
  implicit none

contains

  module character(len=20) function euler0021()
    write (euler0021, "(i20)") answer()
  end function euler0021

  elemental integer(i32) function answer()
    integer(i32), parameter :: n = 10000
    integer(i32) :: i

    answer = 0
    do i = 1, n
      if (spd(spd(i)) == i .and. spd(i) /= i) answer = answer + i
    end do
  end function answer

  elemental integer(i32) function spd(n)
    integer(i32), intent(in) :: n
    integer(i32) :: i

    spd = 1
    do i = 2, int(sqrt(real(n, sp)))
      if (mod(n, i) == 0) spd = merge(spd + i, spd + i + n/i, n/i == i)
    end do
  end function spd

end submodule euler_problem_0021_submodule

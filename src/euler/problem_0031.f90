submodule(interface_module) euler_problem_0031_submodule
  implicit none

contains

  module character(len=20) function euler0031()
    write (euler0031, "(i20)") answer()
  end function euler0031

  elemental integer(i32) function answer()
    integer(i32), parameter :: n = 200
    integer(i32) :: coins(0:7), i, j
    integer(i32) :: array(0:7, 0:n)

    coins = [1, 2, 5, 10, 20, 50, 100, 200]
    array(1:7, :) = 0; array(0, :) = 1

    do j = 0, n
      do i = 1, 7
        associate (next => array(i, j), prev => array(i - 1, j))
          next = prev
          if (j >= coins(i)) next = next + array(i, j - coins(i))
        end associate
      end do
    end do
    answer = array(7, n)
  end function answer

end submodule euler_problem_0031_submodule

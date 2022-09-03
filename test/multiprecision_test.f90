program test

  use multiprecision_module
  use constant_module
  implicit none

  type(long_integer) :: i1, i2, i3

  print "(a)", "Testing"

  test_init: block
    integer(i32) :: i
    logical :: cond

    i1 = [(i, i=1, 9)]
    i2 = "123456789"
    i3 = 123456789_int32
    i1 = int([(i, i=1, 9)], "ll")
    i2 = int("123456789", "ll")
    i3 = int(123456789, "ll")
    print "(a)", "Testing initialization... passed."
  end block test_init

  test_power: block
    character(:), allocatable :: str
    
    str = "437124189620885610010004822109262358637075660656881926429"
    i1 = 123456789
    i3 = int(str, "ll")
    if (i3 == i1**7) then
      print "(a)", "Testing operator(**)... passed."
    else
      print "(a)", "Testing operator(**)... not passed!"
      error stop
    end if
  end block test_power

end program test

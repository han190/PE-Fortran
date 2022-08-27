program test

  use long_integer_module
  use constant_module
  implicit none

  type(long_integer) :: i1, i2, i3

  test_init: block
    integer(i32) :: i
    logical :: cond

    i1 = [(i, i=1, 9)]
    i2 = "123456789"
    i3 = 123456789_int32
    i1 = int([(i, i=1, 9)], "ll")
    i2 = int("123456789", "ll")
    i3 = int(123456789_int32, "ll")
  end block test_init

end program test

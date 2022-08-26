program test

  use long_integer_module
  implicit none

  type(long_integer) :: a, b, c

  a = '23154234952093487'
  b = '91220349520349534'
  c = a + b
  print "(a, *(i1))", c%sign, c%digit

end program test
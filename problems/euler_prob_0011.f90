SUBMODULE(euler_problems) euler_problem_0011
  ! Project Euler: Problem 0011
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0011()
    IMPLICIT NONE
    CHARACTER(len=20) :: file_name
    INTEGER(li), DIMENSION(20, 20) :: int_arr
    INTEGER(li), DIMENSION(4, 4) :: the_box
    INTEGER(li) :: i, j, prod_max
    file_name = 'euler0011.txt'
    OPEN (unit=2, file=file_name, status='old', action='read')
    READ (2, *) int_arr
    CLOSE (2)
    prod_max = 0_li
    outer: DO i = 1_li, 17_li
       inner: DO j = 1_li, 17_li
          the_box = int_arr(i:i + 3_li, j:j + 3_li)
          IF (max_block(the_box) > prod_max) prod_max = max_block(the_box)
       END DO inner
    END DO outer
    WRITE (euler0011, "(i20)") prod_max
  END FUNCTION euler0011
  !
  INTEGER(li) FUNCTION max_block(m)
    IMPLICIT NONE
    INTEGER(li), DIMENSION(:, :), INTENT(in) :: m
    INTEGER(li), DIMENSION(4) :: d1, d2
    INTEGER :: k
    DO k = 1, 4
       d1(k) = m(k, k)
       d2(k) = m(k, 5 - k)
    END DO
    max_block = MAX(PRODUCT(m(1:4, 1)), PRODUCT(m(1:4, 4)), &
         PRODUCT(m(1, 1:4)), PRODUCT(m(4, 1:4)), PRODUCT(d1(1:4)), &
         PRODUCT(d2(1:4)))
  END FUNCTION max_block
END SUBMODULE euler_problem_0011

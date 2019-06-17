SUBMODULE(euler_problems) euler_problem_0018
  ! Project Euler: Problem 0018
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0018()
    IMPLICIT NONE
    CHARACTER(len=20) :: file_name
    TYPE(tri_matrix), DIMENSION(:), ALLOCATABLE :: b
    INTEGER(li), PARAMETER :: n = 15_li
    INTEGER(li) :: i, j
    file_name = 'euler0018.txt'
    ALLOCATE (b(n))
    DO i = 1_li, n
       ALLOCATE (b(i)%a(i))
    END DO
    OPEN (unit=5, file=file_name, status='old', action='read')
    DO i = 1_li, n
       READ (5, *) b(i)%a
    END DO
    CLOSE (5)
    DO j = n - 1_li, 1_li, -1_li
       DO i = 1_li, j
          b(j)%a(i) = MAX(b(j + 1_li)%a(i), b(j + 1_li)%a(i + 1_li)) &
               + b(j)%a(i)
       END DO
    END DO
    WRITE (euler0018, "(i20)") b(1)%a(1)
  END FUNCTION euler0018
END SUBMODULE euler_problem_0018

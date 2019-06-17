SUBMODULE(euler_problems) euler_problem_0044
  ! Project Euler: Problem 0044
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER(ll), PARAMETER :: dimension_of_array = 3000_ll
  INTEGER(ll), DIMENSION(dimension_of_array) :: pentagonal_array
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0044()
    IMPLICIT NONE
    INTEGER(ll) :: i, j, minimised_D
    INTEGER(ll) :: Pm, Pn
    j = 1_ll; i = 1_ll
    DO WHILE (i <= dimension_of_array)
       IF (is_pentagonal(j)) THEN
          pentagonal_array(i) = j
          i = i + 1_ll
       END IF
       j = j + 1_ll
    END DO
    ! set it to a relatively saying big number
    minimised_D = HUGE(0_ll)
    DO i = 1_ll, dimension_of_array
       DO j = i + 1_ll, dimension_of_array
          Pm = pentagonal_array(i)
          Pn = pentagonal_array(j)
          IF (is_pentagonal_sum_diff(Pm, Pn)) THEN
             minimised_D = MIN(minimised_D, Pn - Pm)
          END IF
       END DO
    END DO
    WRITE (euler0044, "(i20)") minimised_D
  END FUNCTION euler0044
  !
  LOGICAL FUNCTION is_pentagonal_sum_diff(Pj, Pk)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: Pj, Pk
    INTEGER(ll) :: S, D
    IF (Pj > Pk) THEN
       ERROR STOP "IS_PENTAGONAL_SUM_DIFF: It is necessary that Pj < Pk."
    END IF
    S = Pj + Pk; D = Pk - Pj
    IF (is_pentagonal(S) .AND. is_pentagonal(D)) THEN
       is_pentagonal_sum_diff = .TRUE.
    ELSE
       is_pentagonal_sum_diff = .FALSE.
    END IF
  END FUNCTION is_pentagonal_sum_diff
  !
  LOGICAL FUNCTION is_pentagonal(P)
    USE euler_utility, ONLY: is_integer
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: P
    IF (p <= 0) THEN
       ERROR STOP "IS_PENTAGONAL: Input number has to be positive."
    END IF
    ASSOCIATE (x=>SQRT(24.0_dp*REAL(P, dp) + 1.0_dp))
      IF (is_integer(x) .AND. MOD(INT(x, ll), 6_ll) == 5_ll) THEN
         is_pentagonal = .TRUE.
      ELSE
         is_pentagonal = .FALSE.
      END IF
    END ASSOCIATE
  END FUNCTION is_pentagonal
END SUBMODULE euler_problem_0044

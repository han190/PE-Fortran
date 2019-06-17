PROGRAM main
  ! Project Euler: Main program
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  USE iso_fortran_env, ONLY: COMPILER_VERSION
  USE euler_problems
  IMPLICIT NONE
  INTEGER, PARAMETER :: DOA = 50 ! Numbers of problems finished
  CHARACTER(len=20), DIMENSION(DOA) :: ans
  CHARACTER(len=100) :: hyperlink
  REAL, DIMENSION(DOA) :: tspan
  PROCEDURE(euler0001), POINTER :: p
  INTEGER :: i
  OPEN (47, file='README.md')
  WRITE (47, "(a)") "# Compilers"
  WRITE (47, *) ""
  WRITE (47, *) COMPILER_VERSION()
  WRITE (47, *) ""
  WRITE (47, "(a)") "# Answers and Benchmarks"
  WRITE (47, "(a)") "|Problem|Answer|Time Span(s)|"
  WRITE (47, "(a)") "|:---|:---|:---|"
  DO i = 1, DOA
     CALL associate_procedure_pointer(i, p)
     CALL compute_answer(p, ans(i), tspan(i))
     WRITE (hyperlink, "(a, i4.4, a, i4.4, a)") "[", &
          i, "](https://gitlab.com/CaptainSolo/project-euler&
          &/blob/master/problems/euler_prob_", i, ".f90)"
     WRITE (47, 1120) " | ", TRIM(hyperlink), " | ", &
          TRIM(ans(i)), " | ", tspan(i), " | "
  END DO
  WRITE (47, *) " | ", " | ", " Total time ", " | ", SUM(tspan), " | "
  CLOSE (47)
1120 FORMAT(a, a, a, a20, a, f10.6, a)
CONTAINS
  SUBROUTINE compute_answer(proc, answer, time)
    IMPLICIT NONE
    PROCEDURE(euler0001), POINTER, INTENT(in) :: proc
    CHARACTER(len=20), INTENT(out) :: answer
    REAL, INTENT(out) :: time
    REAL :: ti, tf
    IF (.NOT. ASSOCIATED(proc)) THEN
       ERROR STOP "COMPUTE_ANSWER: &
            & Procedure pointer must be associated first."
    ELSE
       CALL CPU_TIME(ti)
       answer = proc()
       CALL CPU_TIME(tf)
       time = tf - ti
    END IF
  END SUBROUTINE compute_answer
  !
  SUBROUTINE associate_procedure_pointer(n, p)
    IMPLICIT NONE
    INTEGER, INTENT(in) :: n
    PROCEDURE(euler0001), POINTER, INTENT(out) :: p
    !
    SELECT CASE (n)
    CASE (1)
       p => euler0001
    CASE (2)
       p => euler0002
    CASE (3)
       p => euler0003
    CASE (4)
       p => euler0004
    CASE (5)
       p => euler0005
    CASE (6)
       p => euler0006
    CASE (7)
       p => euler0007
    CASE (8)
       p => euler0008
    CASE (9)
       p => euler0009
    CASE (10)
       p => euler0010
    CASE (11)
       p => euler0011
    CASE (12)
       p => euler0012
    CASE (13)
       p => euler0013
    CASE (14)
       p => euler0014
    CASE (15)
       p => euler0015
    CASE (16)
       p => euler0016
    CASE (17)
       p => euler0017
    CASE (18)
       p => euler0018
    CASE (19)
       p => euler0019
    CASE (20)
       p => euler0020
    CASE (21)
       p => euler0021
    CASE (22)
       p => euler0022
    CASE (23)
       p => euler0023
    CASE (24)
       p => euler0024
    CASE (25)
       p => euler0025
    CASE (26)
       p => euler0026
    CASE (27)
       p => euler0027
    CASE (28)
       p => euler0028
    CASE (29)
       p => euler0029
    CASE (30)
       p => euler0030
    CASE (31)
       p => euler0031
    CASE (32)
       p => euler0032
    CASE (33)
       p => euler0033
    CASE (34)
       p => euler0034
    CASE (35)
       p => euler0035
    CASE (36)
       p => euler0036
    CASE (37)
       p => euler0037
    CASE (38)
       p => euler0038
    CASE (39)
       p => euler0039
    CASE (40)
       p => euler0040
    CASE (41)
       p => euler0041
    CASE (42)
       p => euler0042
    CASE (43)
       p => euler0043
    CASE (44)
       p => euler0044
    CASE (45)
       p => euler0045
    CASE (46)
       p => euler0046
    CASE (47)
       p => euler0047
    CASE (48)
       p => euler0048
    CASE (49)
       p => euler0049
    CASE (50)
       p => euler0050
    CASE DEFAULT
       ERROR STOP "ASSOCIATE_PROCEDURE_POINTER: &
            & There is no corresponding case."
    END SELECT
  END SUBROUTINE associate_procedure_pointer
END PROGRAM main

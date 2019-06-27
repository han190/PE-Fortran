SUBMODULE(euler_problems) euler_problem_0051
  ! Project Euler: Problem 0051
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  !
  ! The function next_permutation is very necessary, some answer online might
  ! just use a manual list to replace it. The result is fast but it is kinda
  ! cheating cuz the problem says "by replacing part of the number", so you do
  ! not know which digits are replaced. The only way to figure out is to list
  ! all the possibilities, which is permutation.
  !
  ! Some further optimizations could be done for this problem. One could
  ! construct a logical function called next_logic_permute and output the next
  ! permute by boolean variables, which should save a lot of memory. I have not
  ! figure out how to best implement that yet.
  !-----------------------------------------------------------------------------
  IMPLICIT NONE
CONTAINS
  MODULE CHARACTER(len=20) FUNCTION euler0051()
    USE euler_utility, ONLY: sieve_of_Eratosthenes, unit_digit
    IMPLICIT NONE
    INTEGER(li) :: n, i, j, k, test_int
    INTEGER(li) :: u, u_min, u_max, v
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: int_arr, index_arr
    INTEGER(li), DIMENSION(6) :: not_prime
    not_prime = [0, 2, 4, 5, 6, 8]
    n = 999999
    CALL sieve_of_Eratosthenes(n, is_prime)
    index_arr = [0, 0, 0, 0, 0, 0]
    outermost: DO v = 6, 1, -1
       index_arr(v:) = 1
       IF (ALLOCATED(int_arr)) DEALLOCATE (int_arr)
       ALLOCATE (int_arr(SIZE(index_arr)))
       n = COUNT(index_arr == 0, dim=1)
       u_min = 10**(n - 1)
       u_max = 10**n - 1
       outer: DO WHILE (next_permutation(index_arr))
          inner: DO u = u_min, u_max
             IF (ANY(not_prime == unit_digit(u)) &
                  .AND. index_arr(SIZE(index_arr)) == 0) CYCLE inner
             int_arr = 0; k = u
             DO i = SIZE(index_arr), 1, -1
                IF (index_arr(i) == 0) THEN
                   int_arr(i) = unit_digit(k)
                   k = k/10
                END IF
             END DO
             j = 0
             innermost: DO i = 0, 9
                test_int = arr2int(int_arr + index_arr*i)
                IF (.NOT. is_prime(test_int)) j = j + 1
                IF (j == 3) THEN
                   EXIT innermost
                ELSE IF (i == 9) THEN
                   EXIT outermost
                END IF
             END DO innermost
          END DO inner
       END DO outer
    END DO outermost
    DO i = SIZE(index_arr), 1, -1
       IF (index_arr(i) == 0) THEN
          index_arr(i) = unit_digit(u)
          u = u/10
       END IF
    END DO
    WRITE (euler0051, "(i20)") arr2int(index_arr)
  END FUNCTION euler0051
  !
  LOGICAL FUNCTION next_permutation(input_arr)
    use euler_utility, only: swap 
    IMPLICIT NONE
    INTEGER(li), DIMENSION(:), INTENT(inout) :: input_arr
    INTEGER(li) :: i, j
    next_permutation = .FALSE.
    i = SIZE(input_arr)
    DO WHILE (i > 1 .AND. input_arr(i - 1) >= input_arr(i))
       i = i - 1
    END DO
    IF (i <= 1) THEN
       next_permutation = .FALSE.
       RETURN
    END IF
    j = SIZE(input_arr)
    DO WHILE (input_arr(j) <= input_arr(i - 1))
       j = j - 1
    END DO
    CALL swap(input_arr(i - 1), input_arr(j))
    input_arr(i:) = input_arr(SIZE(input_arr):i:-1)
    next_permutation = .TRUE.
  END FUNCTION next_permutation
  !
  INTEGER FUNCTION arr2int(arr)
    IMPLICIT NONE
    INTEGER, DIMENSION(:), INTENT(in) :: arr
    INTEGER :: i, tmp
    tmp = 0
    DO i = 1, SIZE(arr)
       tmp = tmp + arr(i)*10**(SIZE(arr) - i)
    END DO
    arr2int = tmp
  END FUNCTION arr2int
END SUBMODULE euler_problem_0051

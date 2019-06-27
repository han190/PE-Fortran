PROGRAM main
  USE euler_utility, ONLY: li, sieve_of_Eratosthenes, unit_digit
  IMPLICIT NONE
  INTEGER(li) :: n, i, j, k, u, v, test_int, u_min, u_max
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
  INTEGER(li), ALLOCATABLE, DIMENSION(:) :: int_arr, index_arr
  INTEGER(li), DIMENSION(6) :: not_prime
  not_prime = [0, 2, 4, 5, 6, 8]
  n = 999999_li
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
            !   IF (is_prime(test_int)) j = j + 1
            !   IF (j >= 8) THEN
            !      EXIT outermost
            !   END IF
              IF (.not. is_prime(test_int)) j = j + 1 
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
  WRITE (*, *) arr2int(index_arr)
CONTAINS
  LOGICAL FUNCTION next_permutation(input_arr)
    IMPLICIT NONE
    INTEGER(li), DIMENSION(:), INTENT(inout) :: input_arr
    INTEGER(li) :: i, j
    next_permutation = .FALSE.
    i = SIZE(input_arr)
    DO WHILE (i > 1_li .AND. input_arr(i - 1_li) >= input_arr(i))
       i = i - 1_li
    END DO
    IF (i <= 1_li) THEN
       next_permutation = .FALSE.
       RETURN
    END IF
    j = SIZE(input_arr)
    DO WHILE (input_arr(j) <= input_arr(i - 1))
       j = j - 1_li
    END DO
    CALL swap(input_arr(i - 1_li), input_arr(j))
    input_arr(i:) = input_arr(SIZE(input_arr):i:-1)
    next_permutation = .TRUE.
  END FUNCTION next_permutation

  SUBROUTINE swap(a, b)
    IMPLICIT NONE
    INTEGER(li), INTENT(inout) :: a, b
    INTEGER(li) :: tmp
    tmp = a; a = b; b = tmp
  END SUBROUTINE swap

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
END PROGRAM main

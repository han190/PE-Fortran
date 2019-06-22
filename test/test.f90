PROGRAM main
  USE euler_utility, ONLY: li, sieve_of_Eratosthenes, pop, int_2_arr, append
  IMPLICIT NONE
  INTEGER(li) :: n, i, j
  LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
  INTEGER(li), ALLOCATABLE, DIMENSION(:) :: prime_arr, arr, int_arr, ans_arr
  INTEGER(li), ALLOCATABLE, DIMENSION(:, :) :: prime_block

  n = 999999_li
  CALL sieve_of_Eratosthenes(n, is_prime)
  ALLOCATE (prime_arr(COUNT(is_prime)))
  j = 1_li
  DO i = 2_li, n
     IF (is_prime(i)) THEN
        prime_arr(j) = i
        j = j + 1_li
     END IF
  END DO
  arr = PACK(prime_arr, prime_arr >= 100000_li .AND. prime_arr <= 999999_li)
  CALL int_2_arr(arr(1), int_arr)
  ALLOCATE (prime_block(SIZE(int_arr), SIZE(arr)))
  DO i = 1, SIZE(arr)
     CALL int_2_arr(arr(i), int_arr)
     prime_block(1:SIZE(int_arr), i) = int_arr(:)
  END DO
  DO i = 1, SIZE(arr)
     IF (prime_block(5, i) == prime_block(1, i) .AND. &
          prime_block(2, i) == 2 .AND. &
          prime_block(4, i) == 3 .AND. &
          prime_block(6, i) == 3 .and. &
          prime_block(1, i) == prime_block(3, i)) THEN
        CALL append(ans_arr, arr(i))
     END IF
  END DO
  print*, ans_arr
  print*, size(pack(prime_block(2, :), prime_block(2, :) == 2))
END PROGRAM main

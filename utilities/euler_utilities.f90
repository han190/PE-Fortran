MODULE euler_utility
  ! Project Euler: The library of functions and subroutines
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  USE iso_fortran_env, ONLY: int32, int64, real32, real64
  IMPLICIT NONE
  INTEGER, PARAMETER :: li = int32
  INTEGER, PARAMETER :: ll = int64
  INTEGER, PARAMETER :: sp = real32
  INTEGER, PARAMETER :: dp = real64
  !
  REAL(sp), PARAMETER :: tiny_sp = TINY(0._sp)
  REAL(dp), PARAMETER :: tiny_dp = TINY(0._dp)
  ! Import functions and subrotuines from submodules 
  INTERFACE
     !
     MODULE LOGICAL FUNCTION is_prime_li(n)
       IMPLICIT NONE
       INTEGER(li), INTENT(in) :: n
       INTEGER(li) :: limit, i
     END FUNCTION is_prime_li
     !
     MODULE LOGICAL FUNCTION is_prime_ll(n)
       IMPLICIT NONE
       INTEGER(ll), INTENT(in) :: n
       INTEGER(ll) :: limit, i
     END FUNCTION is_prime_ll
     !
     MODULE SUBROUTINE sieve_of_Eratosthenes_li(n, is_prime)
       IMPLICIT NONE
       INTEGER(li), INTENT(in) :: n
       LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
       INTEGER(li) :: i, j
     END SUBROUTINE sieve_of_Eratosthenes_li
     !
     MODULE SUBROUTINE sieve_of_Eratosthenes_ll(n, is_prime)
       IMPLICIT NONE
       INTEGER(ll), INTENT(in) :: n
       LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
       INTEGER(ll) :: i, j
     END SUBROUTINE sieve_of_Eratosthenes_ll
     !
     MODULE SUBROUTINE lexical_sort(str_arr, case_insensitive)
      CHARACTER(len=*), DIMENSION(:), INTENT(inout) :: str_arr
      LOGICAL, INTENT(in), OPTIONAL :: case_insensitive
      INTEGER :: low, high, ios, k
     END SUBROUTINE lexical_sort
  END INTERFACE
  ! Interface for generic functions
  INTERFACE is_integer
     MODULE PROCEDURE is_integer_sp, is_integer_dp
  END INTERFACE is_integer
  !
  INTERFACE factorial
     MODULE PROCEDURE factorial_li, factorial_ll
  END INTERFACE factorial
  !
  INTERFACE unit_digit
     MODULE PROCEDURE unit_digit_li, unit_digit_ll
  END INTERFACE unit_digit
  !
  INTERFACE reverse
     MODULE PROCEDURE reverse_li, reverse_ll
  END INTERFACE reverse
  !
  INTERFACE is_palindromic
     MODULE PROCEDURE is_palindromic_li, is_palindromic_ll
  END INTERFACE is_palindromic
  !
  INTERFACE length_of_int
     MODULE PROCEDURE length_of_int_li, length_of_int_ll
  END INTERFACE length_of_int
  !
  INTERFACE is_pandigital
     MODULE PROCEDURE is_pandigital_li, is_pandigital_ll
  END INTERFACE is_pandigital
  !
  INTERFACE gcd
     MODULE PROCEDURE gcd_li, gcd_ll
  END INTERFACE gcd
  !
  INTERFACE lcm
     MODULE PROCEDURE lcm_li, lcm_ll
  END INTERFACE lcm
  !
  INTERFACE fibonacci
     MODULE PROCEDURE fibonacci_li, fibonacci_ll
  END INTERFACE fibonacci
  !
  INTERFACE int_2_arr
     MODULE PROCEDURE int_2_arr_li, int_2_arr_ll
  END INTERFACE int_2_arr
  !
  INTERFACE carry
     MODULE PROCEDURE carry_add, carry_multiply
  END INTERFACE carry
  !
  INTERFACE remove_duplicates
     MODULE PROCEDURE remove_duplicates_1d_li, remove_duplicates_1d_ll
     MODULE PROCEDURE remove_duplicates_1d_sp, remove_duplicates_1d_dp
  END INTERFACE remove_duplicates
  !
  INTERFACE append
     MODULE PROCEDURE append_sp, append_dp, append_li, append_ll
  END INTERFACE append
  !
  INTERFACE pop
     MODULE PROCEDURE pop_sp, pop_dp, pop_li, pop_ll
  END INTERFACE pop
  !
  INTERFACE is_prime
     MODULE PROCEDURE is_prime_ll, is_prime_li
  END INTERFACE is_prime
  !
  INTERFACE sieve_of_Eratosthenes
     MODULE PROCEDURE sieve_of_Eratosthenes_li, sieve_of_Eratosthenes_ll
  END INTERFACE sieve_of_Eratosthenes
  ! Strict the usage of functions and subroutines 
  PRIVATE :: is_integer_sp, is_integer_dp
  PRIVATE :: unit_digit_li, unit_digit_ll, factorial_li, factorial_ll
  PRIVATE :: is_prime_ll, is_prime_li, reverse_li, reverse_ll
  PRIVATE :: is_palindromic_li, is_palindromic_ll
  PRIVATE :: length_of_int_li, length_of_int_ll
  PRIVATE :: is_pandigital_li, is_pandigital_ll
  PRIVATE :: gcd_li, gcd_ll, lcm_li, lcm_ll, fibonacci_li, fibonacci_ll
  PRIVATE :: int_2_arr_li, int_2_arr_ll, carry_add, carry_multiply
  PRIVATE :: remove_duplicates_1d_li, remove_duplicates_1d_ll
  PRIVATE :: remove_duplicates_1d_sp, remove_duplicates_1d_dp
  PRIVATE :: append_sp, append_dp, append_li, append_ll
  PRIVATE :: pop_sp, pop_dp, pop_li, pop_ll
  PRIVATE :: sieve_of_Eratosthenes_li, sieve_of_Eratosthenes_ll
CONTAINS
  !
  LOGICAL FUNCTION is_integer_sp(n)
    IMPLICIT NONE
    REAL(sp), INTENT(in) :: n
    IF (ABS(REAL(FLOOR(n, li), sp) - n) < tiny_sp) THEN
       is_integer_sp = .TRUE.
    ELSE
       is_integer_sp = .FALSE.
    END IF
  END FUNCTION is_integer_sp
  !
  LOGICAL FUNCTION is_integer_dp(n)
    IMPLICIT NONE
    REAL(dp), INTENT(in) :: n
    IF (ABS(REAL(FLOOR(n, li), dp) - n) < tiny_dp) THEN
       is_integer_dp = .TRUE.
    ELSE
       is_integer_dp = .FALSE.
    END IF
  END FUNCTION is_integer_dp
  !
  INTEGER(li) FUNCTION factorial_li(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li) :: i, temp
    IF (n >= 13_li) THEN
       ERROR STOP "The input integer is greater than 12, &
            & thus the factorial execeds the limit of a 32 bit integer."
    END IF
    temp = 1_li
    DO i = 1_li, n
       temp = temp*i
    END DO
    factorial_li = temp
  END FUNCTION factorial_li
  !
  INTEGER(ll) FUNCTION factorial_ll(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll) :: i, temp
    IF (n >= 21_ll) THEN
       ERROR STOP "The input integer is greater than 20, &
            & thus the factorial execeds the limit of a 64 bit integer."
    END IF
    temp = 1_ll
    DO i = 1_ll, n
       temp = temp*i
    END DO
    factorial_ll = temp
  END FUNCTION factorial_ll
  !
  INTEGER(li) FUNCTION unit_digit_li(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    unit_digit_li = n - n/10_li*10_li
  END FUNCTION unit_digit_li
  !
  INTEGER(ll) FUNCTION unit_digit_ll(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    unit_digit_ll = n - n/10_ll*10_ll
  END FUNCTION unit_digit_ll
  !
  INTEGER(li) FUNCTION reverse_li(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li) :: reversed, ntemp
    reversed = 0_li; ntemp = n
    DO WHILE (ntemp > 0_li)
       reversed = reversed*10_li + MOD(ntemp, 10_li)
       ntemp = ntemp/10_li
    END DO
    reverse_li = reversed
  END FUNCTION reverse_li
  !
  INTEGER(ll) FUNCTION reverse_ll(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll) :: reversed, ntemp
    reversed = 0_ll; ntemp = n
    DO WHILE (ntemp > 0_ll)
       reversed = reversed*10_ll + MOD(ntemp, 10_ll)
       ntemp = ntemp/10_ll
    END DO
    reverse_ll = reversed
  END FUNCTION reverse_ll
  !
  LOGICAL FUNCTION is_palindromic_li(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    is_palindromic_li = .FALSE.
    IF (n == reverse_li(n)) is_palindromic_li = .TRUE.
  END FUNCTION is_palindromic_li
  !
  LOGICAL FUNCTION is_palindromic_ll(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    is_palindromic_ll = .FALSE.
    IF (n == reverse_ll(n)) is_palindromic_ll = .TRUE.
  END FUNCTION is_palindromic_ll
  !
  INTEGER(li) FUNCTION length_of_int_li(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    length_of_int_li = FLOOR(LOG10(REAL(n, sp)), li) + 1_li
  END FUNCTION length_of_int_li
  !
  INTEGER(ll) FUNCTION length_of_int_ll(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    length_of_int_ll = FLOOR(LOG10(REAL(n, sp)), ll) + 1_ll
  END FUNCTION length_of_int_ll
  !
  LOGICAL FUNCTION is_pandigital_ll(n, digs)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll), OPTIONAL, INTENT(in) :: digs
    INTEGER(ll) :: temp, j, length
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: logical_array
    IF (PRESENT(digs)) THEN
       ALLOCATE (logical_array(digs))
       length = digs
    ELSE
       length = 9_ll
       ALLOCATE (logical_array(length))
    END IF
    is_pandigital_ll = .FALSE.; logical_array = .FALSE.; temp = n
    DO
       j = temp - temp/10_ll*10_ll
       IF (j == 0_ll .OR. j > length) EXIT
       logical_array(j) = .TRUE.
       temp = temp/10_ll
    END DO
    IF (COUNT(logical_array) == length) THEN
       is_pandigital_ll = .TRUE.
    ELSE
       is_pandigital_ll = .FALSE.
    END IF
  END FUNCTION is_pandigital_ll
  !
  LOGICAL FUNCTION is_pandigital_li(n, digs)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li), OPTIONAL, INTENT(in) :: digs
    INTEGER(li) :: temp, j, length
    LOGICAL, ALLOCATABLE, DIMENSION(:) :: logical_array
    IF (PRESENT(digs)) THEN
       ALLOCATE (logical_array(digs))
       length = digs
    ELSE
       length = 9_li
       ALLOCATE (logical_array(length))
    END IF
    is_pandigital_li = .FALSE.; logical_array = .FALSE.; temp = n
    DO
       j = temp - temp/10_li*10_li
       IF (j == 0_li .OR. j > length) EXIT
       logical_array(j) = .TRUE.
       temp = temp/10_li
    END DO
    IF (COUNT(logical_array) == length) THEN
       is_pandigital_li = .TRUE.
    ELSE
       is_pandigital_li = .FALSE.
    END IF
  END FUNCTION is_pandigital_li
  !
  RECURSIVE FUNCTION gcd_li(n1, n2) RESULT(ans)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n1, n2
    INTEGER(li) :: ans
    IF (n2 == 0_li) THEN
       ans = n1
       RETURN
    ELSE
       ans = gcd_li(n2, MOD(n1, n2))
       RETURN
    END IF
  END FUNCTION gcd_li
  !
  RECURSIVE FUNCTION gcd_ll(n1, n2) RESULT(ans)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n1, n2
    INTEGER(ll) :: ans
    IF (n2 == 0_ll) THEN
       ans = n1
       RETURN
    ELSE
       ans = gcd_ll(n2, MOD(n1, n2))
       RETURN
    END IF
  END FUNCTION gcd_ll
  !
  INTEGER(li) FUNCTION lcm_li(n1, n2)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n1, n2
    lcm_li = n1*n2/gcd_li(n1, n2)
  END FUNCTION lcm_li
  !
  INTEGER(ll) FUNCTION lcm_ll(n1, n2)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n1, n2
    lcm_ll = n1*n2/gcd_ll(n1, n2)
  END FUNCTION lcm_ll
  !
  INTEGER(li) FUNCTION fibonacci_li(n)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li), DIMENSION(2) :: f
    INTEGER(li) :: i
    f = [1_li, 2_li]
    IF (MOD(n, 2_li) /= 0_li) THEN
       DO i = 1_li, n/2_li
          f(1) = SUM(f); f(2) = SUM(f)
       END DO
       fibonacci_li = f(1)
       RETURN
    ELSE
       DO i = 1_li, n/2_li - 1_li
          f(1) = SUM(f); f(2) = SUM(f)
       END DO
       fibonacci_li = f(2)
       RETURN
    END IF
  END FUNCTION fibonacci_li
  !
  INTEGER(ll) FUNCTION fibonacci_ll(n)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll), DIMENSION(2) :: f
    INTEGER(ll) :: i
    f = [1_ll, 2_ll]
    IF (MOD(n, 2_ll) /= 0_ll) THEN
       DO i = 1_ll, n/2_ll
          f(1) = SUM(f); f(2) = SUM(f)
       END DO
       fibonacci_ll = f(1)
       RETURN
    ELSE
       DO i = 1_ll, n/2_ll - 1_ll
          f(1) = SUM(f); f(2) = SUM(f)
       END DO
       fibonacci_ll = f(2)
       RETURN
    END IF
  END FUNCTION fibonacci_ll
  !
  SUBROUTINE int_2_arr_li(n, array)
    IMPLICIT NONE
    INTEGER(li), INTENT(in) :: n
    INTEGER(li), ALLOCATABLE, DIMENSION(:), INTENT(out) :: array
    INTEGER(li) :: temp, i
    temp = n
    ALLOCATE (array(length_of_int_li(temp)))
    array = 0_li
    DO i = length_of_int_li(temp), 1_li, -1_li
       array(i) = unit_digit_li(temp)
       temp = temp/10_li
    END DO
  END SUBROUTINE int_2_arr_li
  !
  SUBROUTINE int_2_arr_ll(n, array)
    IMPLICIT NONE
    INTEGER(ll), INTENT(in) :: n
    INTEGER(ll), ALLOCATABLE, DIMENSION(:), INTENT(out) :: array
    INTEGER(ll) :: temp, i
    temp = n
    ALLOCATE (array(length_of_int_ll(temp)))
    array = 0_ll
    DO i = length_of_int_ll(temp), 1_ll, -1_ll
       array(i) = unit_digit_ll(temp)
       temp = temp/10_ll
    END DO
  END SUBROUTINE int_2_arr_ll
  !
  SUBROUTINE carry_multiply(a, m)
    IMPLICIT NONE
    INTEGER(li), DIMENSION(2), INTENT(inout) :: a
    INTEGER(li), INTENT(in) :: m
    INTEGER(li) :: temp
    temp = a(2)
    a(2) = unit_digit_li(a(2))
    a(1) = a(1)*m + (temp - a(2))/10_li
  END SUBROUTINE carry_multiply
  !
  SUBROUTINE carry_add(a)
    IMPLICIT NONE
    INTEGER(li), DIMENSION(2), INTENT(inout) :: a
    INTEGER(li) :: temp
    temp = a(2)
    a(2) = unit_digit_li(a(2))
    a(1) = a(1) + (temp - a(2))/10_li
  END SUBROUTINE carry_add
  !
  SUBROUTINE remove_duplicates_1d_li(array, output_array)
    IMPLICIT NONE
    INTEGER(li), DIMENSION(:), INTENT(in) :: array
    INTEGER(li), ALLOCATABLE, DIMENSION(:), INTENT(out) :: output_array
    INTEGER(li), DIMENSION(SIZE(array)) :: temp_array
    INTEGER(li) :: i, k
    temp_array = 0_li
    k = 1_li; temp_array(1) = array(1)
    DO i = 2_li, SIZE(array)
       IF (ANY(temp_array == array(i))) CYCLE
       k = k + 1_li; temp_array(k) = array(i)
    END DO
    ALLOCATE (output_array(k))
    output_array(:) = temp_array(1:k)
  END SUBROUTINE remove_duplicates_1d_li
  !
  SUBROUTINE remove_duplicates_1d_ll(array, output_array)
    IMPLICIT NONE
    INTEGER(ll), DIMENSION(:), INTENT(in) :: array
    INTEGER(ll), ALLOCATABLE, DIMENSION(:), INTENT(out) :: output_array
    INTEGER(ll), DIMENSION(SIZE(array)) :: temp_array
    INTEGER(ll) :: i, k
    temp_array = 0_ll
    k = 1_ll; temp_array(1) = array(1)
    DO i = 2_ll, SIZE(array)
       IF (ANY(temp_array == array(i))) CYCLE
       k = k + 1_ll; temp_array(k) = array(i)
    END DO
    ALLOCATE (output_array(k))
    output_array(:) = temp_array(1:k)
  END SUBROUTINE remove_duplicates_1d_ll
  !
  SUBROUTINE remove_duplicates_1d_sp(array, output_array)
    IMPLICIT NONE
    REAL(sp), DIMENSION(:), INTENT(in) :: array
    REAL(sp), ALLOCATABLE, DIMENSION(:), INTENT(out) :: output_array
    REAL(sp), DIMENSION(SIZE(array)) :: temp_array
    INTEGER(li) :: i, k
    temp_array = 0.0_sp
    k = 1.0_sp; temp_array(1) = array(1)
    DO i = 2_li, SIZE(array)
       IF (ANY(ABS(temp_array - array(i)) < tiny_sp)) CYCLE
       k = k + 1_li; temp_array(k) = array(i)
    END DO
    ALLOCATE (output_array(k))
    output_array(:) = temp_array(1:k)
  END SUBROUTINE remove_duplicates_1d_sp
  !
  SUBROUTINE remove_duplicates_1d_dp(array, output_array)
    IMPLICIT NONE
    REAL(dp), DIMENSION(:), INTENT(in) :: array
    REAL(dp), ALLOCATABLE, DIMENSION(:), INTENT(out) :: output_array
    REAL(dp), DIMENSION(SIZE(array)) :: temp_array
    INTEGER(li) :: i, k
    temp_array = 0.0_dp
    k = 1.0_dp; temp_array(1) = array(1)
    DO i = 2_li, SIZE(array)
       IF (ANY(ABS(temp_array - array(i)) < tiny_dp)) CYCLE
       k = k + 1_li; temp_array(k) = array(i)
    END DO
    ALLOCATE (output_array(k))
    output_array(:) = temp_array(1:k)
  END SUBROUTINE remove_duplicates_1d_dp
  !
  SUBROUTINE append_sp(array, element)
    IMPLICIT NONE
    REAL(sp), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: array
    REAL(sp), INTENT(in) :: element
    REAL(sp), ALLOCATABLE, DIMENSION(:) :: temp
    IF (ALLOCATED(array)) THEN
       ALLOCATE (temp(SIZE(array)))
       temp(:) = array(:)
       DEALLOCATE (array)
       ALLOCATE (array(SIZE(temp) + 1))
       array(1:SIZE(temp)) = temp(:)
       array(SIZE(temp) + 1) = element
    ELSE
       ALLOCATE (array(1))
       array(1) = element
    END IF
  END SUBROUTINE append_sp
  !
  SUBROUTINE append_dp(array, element)
    IMPLICIT NONE
    REAL(dp), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: array
    REAL(dp), INTENT(in) :: element
    REAL(dp), ALLOCATABLE, DIMENSION(:) :: temp
    IF (ALLOCATED(array)) THEN
       ALLOCATE (temp(SIZE(array)))
       temp(:) = array(:)
       DEALLOCATE (array)
       ALLOCATE (array(SIZE(temp) + 1))
       array(1:SIZE(temp)) = temp(:)
       array(SIZE(temp) + 1) = element
    ELSE
       ALLOCATE (array(1))
       array(1) = element
    END IF
  END SUBROUTINE append_dp
  !
  SUBROUTINE append_li(array, element)
    IMPLICIT NONE
    INTEGER(li), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: array
    INTEGER(li), INTENT(in) :: element
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: temp
    IF (ALLOCATED(array)) THEN
       ALLOCATE (temp(SIZE(array)))
       temp(:) = array(:)
       DEALLOCATE (array)
       ALLOCATE (array(SIZE(temp) + 1))
       array(1:SIZE(temp)) = temp(:)
       array(SIZE(temp) + 1) = element
    ELSE
       ALLOCATE (array(1))
       array(1) = element
    END IF
  END SUBROUTINE append_li
  !
  SUBROUTINE append_ll(array, element)
    IMPLICIT NONE
    INTEGER(ll), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: array
    INTEGER(ll), INTENT(in) :: element
    INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: temp
    IF (ALLOCATED(array)) THEN
       ALLOCATE (temp(SIZE(array)))
       temp(:) = array(:)
       DEALLOCATE (array)
       ALLOCATE (array(SIZE(temp) + 1))
       array(1:SIZE(temp)) = temp(:)
       array(SIZE(temp) + 1) = element
    ELSE
       ALLOCATE (array(1))
       array(1) = element
    END IF
  END SUBROUTINE append_ll
  !
  SUBROUTINE pop_sp(array)
    IMPLICIT NONE
    REAL(sp), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: array
    REAL(sp), ALLOCATABLE, DIMENSION(:) :: temp
    IF (ALLOCATED(array)) THEN
       ALLOCATE (temp(SIZE(array) - 1))
       temp = array(2:SIZE(array))
       DEALLOCATE (array)
       CALL MOVE_ALLOC(temp, array)
    ELSE IF (SIZE(array) == 1) THEN
       DEALLOCATE (array)
    ELSE
       DEALLOCATE (array)
    END IF
  END SUBROUTINE pop_sp
  !
  SUBROUTINE pop_dp(array)
    IMPLICIT NONE
    REAL(dp), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: array
    REAL(dp), ALLOCATABLE, DIMENSION(:) :: temp
    IF (ALLOCATED(array)) THEN
       ALLOCATE (temp(SIZE(array) - 1))
       temp = array(2:SIZE(array))
       DEALLOCATE (array)
       CALL MOVE_ALLOC(temp, array)
    ELSE IF (SIZE(array) == 1) THEN
       DEALLOCATE (array)
    ELSE
       DEALLOCATE (array)
    END IF
  END SUBROUTINE pop_dp
  !
  SUBROUTINE pop_li(array)
    IMPLICIT NONE
    INTEGER(li), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: array
    INTEGER(li), ALLOCATABLE, DIMENSION(:) :: temp
    IF (ALLOCATED(array)) THEN
       ALLOCATE (temp(SIZE(array) - 1))
       temp = array(2:SIZE(array))
       DEALLOCATE (array)
       CALL MOVE_ALLOC(temp, array)
    ELSE IF (SIZE(array) == 1) THEN
       DEALLOCATE (array)
    ELSE
       DEALLOCATE (array)
    END IF
  END SUBROUTINE pop_li
  !
  SUBROUTINE pop_ll(array)
    IMPLICIT NONE
    INTEGER(ll), ALLOCATABLE, DIMENSION(:), INTENT(inout) :: array
    INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: temp
    IF (ALLOCATED(array)) THEN
       ALLOCATE (temp(SIZE(array) - 1_ll))
       temp = array(2_ll:SIZE(array))
       DEALLOCATE (array)
       CALL MOVE_ALLOC(temp, array)
    ELSE IF (SIZE(array) == 1_ll) THEN
       DEALLOCATE (array)
    ELSE
       DEALLOCATE (array)
    END IF
  END SUBROUTINE pop_ll
END MODULE euler_utility

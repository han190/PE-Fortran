MODULE euler_problems
  ! Project Euler: The interface of module functions
  ! Copyright (c) Captain Solo. All rights reserved.
  !-----------------------------------------------------------------------------
  USE euler_utility, ONLY: li, ll, sp, dp
  IMPLICIT NONE
  ! Module type
  TYPE, PRIVATE :: tri_matrix
     INTEGER(li), DIMENSION(:), ALLOCATABLE :: a
  END TYPE tri_matrix
  ! Interface of submodule functions
  INTERFACE
     MODULE CHARACTER(len=20) FUNCTION euler0001()
       IMPLICIT NONE
       INTEGER(li) :: i, j, tmp, answer
     END FUNCTION euler0001
     !
     MODULE CHARACTER(len=20) FUNCTION euler0002()
       USE euler_utility, ONLY: fibonacci
       IMPLICIT NONE
       INTEGER(li) :: i, sum
     END FUNCTION euler0002
     !
     MODULE CHARACTER(len=20) FUNCTION euler0003()
       USE euler_utility, ONLY: sieve_of_Eratosthenes
       IMPLICIT NONE
       INTEGER(ll) :: n, fmax, i
       LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
     END FUNCTION euler0003
     !
     MODULE CHARACTER(len=20) FUNCTION euler0004()
       USE euler_utility, ONLY: is_palindromic
       IMPLICIT NONE
       INTEGER(li) :: a, b, p_max
     END FUNCTION euler0004
     !
     MODULE CHARACTER(len=20) FUNCTION euler0005()
       USE euler_utility, ONLY: lcm
       IMPLICIT NONE
       INTEGER(ll) :: n, i, temp
     END FUNCTION euler0005
     !
     MODULE CHARACTER(len=20) FUNCTION euler0006()
       IMPLICIT NONE
       INTEGER(li) :: n, i, sum_square, square_sum
     END FUNCTION euler0006
     !
     MODULE CHARACTER(len=20) FUNCTION euler0007()
       USE euler_utility, ONLY: is_prime
       IMPLICIT NONE
       INTEGER(li) :: i, j
     END FUNCTION euler0007
     !
     MODULE CHARACTER(len=20) FUNCTION euler0008()
       IMPLICIT NONE
       CHARACTER(len=20) :: file_name
       INTEGER(ll), DIMENSION(1000_ll) :: long_int
       INTEGER(ll) :: i, s, temp
     END FUNCTION euler0008
     !
     MODULE CHARACTER(len=20) FUNCTION euler0009()
       IMPLICIT NONE
       INTEGER(li) :: a, b, c, n
     END FUNCTION euler0009
     !
     MODULE CHARACTER(len=20) FUNCTION euler0010()
       USE euler_utility, ONLY: sieve_of_Eratosthenes
       IMPLICIT NONE
       INTEGER(ll) :: i, temp
       LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
     END FUNCTION euler0010
     !
     MODULE CHARACTER(len=20) FUNCTION euler0011()
       IMPLICIT NONE
       CHARACTER(len=20) :: file_name
       INTEGER(li), DIMENSION(20, 20) :: int_arr
       INTEGER(li), DIMENSION(4, 4) :: the_box
       INTEGER(li) :: i, j, prod_max
     END FUNCTION euler0011
     !
     MODULE CHARACTER(len=20) FUNCTION euler0012()
       IMPLICIT NONE
       INTEGER(li) :: j, k
     END FUNCTION euler0012
     !
     MODULE CHARACTER(len=20) FUNCTION euler0013()
       IMPLICIT NONE
       CHARACTER(len=20) :: file_name
       INTEGER(ll), DIMENSION(50, 100) :: long_int
       INTEGER(ll), DIMENSION(50) :: temp
       INTEGER(ll) :: i, j, k, t
     END FUNCTION euler0013
     !
     MODULE CHARACTER(len=20) FUNCTION euler0014()
       IMPLICIT NONE
       INTEGER(ll) :: k, i, temp
     END FUNCTION euler0014
     !
     MODULE CHARACTER(len=20) FUNCTION euler0015()
       IMPLICIT NONE
       INTEGER(ll) :: i, temp
     END FUNCTION euler0015
     !
     MODULE CHARACTER(len=20) FUNCTION euler0016()
       USE euler_utility, ONLY: carry
       IMPLICIT NONE
       INTEGER(li), ALLOCATABLE, DIMENSION(:) :: iarr
       INTEGER(li) :: l, i, j
     END FUNCTION euler0016
     !
     MODULE CHARACTER(len=20) FUNCTION euler0017()
       IMPLICIT NONE
       INTEGER(li) :: total_count, i
     END FUNCTION euler0017
     !
     MODULE CHARACTER(len=20) FUNCTION euler0018()
       IMPLICIT NONE
       CHARACTER(len=20) :: file_name
       TYPE(tri_matrix), DIMENSION(:), ALLOCATABLE :: b
       INTEGER(li), PARAMETER :: n = 15_li
       INTEGER(li) :: i, j
     END FUNCTION euler0018
     !
     MODULE CHARACTER(len=20) FUNCTION euler0019()
       IMPLICIT NONE
       INTEGER(li), DIMENSION(12) :: month_arr
       INTEGER(li) ::  dow, temp, i, j
     END FUNCTION euler0019
     !
     MODULE CHARACTER(len=20) FUNCTION euler0020()
       USE euler_utility, ONLY: carry
       IMPLICIT NONE
       INTEGER(li), ALLOCATABLE, DIMENSION(:) :: int_arr
       INTEGER(li) :: l, i, j
       REAL(sp) :: lr
     END FUNCTION euler0020
     !
     MODULE CHARACTER(len=20) FUNCTION euler0021()
       IMPLICIT NONE
       INTEGER(li) :: p, t
     END FUNCTION euler0021
     !
     MODULE CHARACTER(len=20) FUNCTION euler0022()
       USE euler_utility, ONLY: lexical_sort
       CHARACTER(len=20) :: file_name
       CHARACTER(len=20), DIMENSION(6000) :: array_of_names
       INTEGER :: stat, i
       INTEGER(ll) :: temp
     END FUNCTION euler0022
     !
     MODULE CHARACTER(len=20) FUNCTION euler0023()
       IMPLICIT NONE
       INTEGER(li), PARAMETER :: limit = 28123_li
       INTEGER(li), DIMENSION(limit) :: sums_of_divs, abundant_draft, nums
       INTEGER(li), ALLOCATABLE, DIMENSION(:) :: abundant_arr
       INTEGER(li) :: i, j
       LOGICAL, DIMENSION(limit) :: can_be_written
     END FUNCTION euler0023
     !
     MODULE CHARACTER(len=20) FUNCTION euler0024()
       IMPLICIT NONE
       INTEGER(ll), DIMENSION(10) :: factor_array
       INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: arr1, arr2
       INTEGER(ll) :: n, temp, i, j, k
     END FUNCTION euler0024
     !
     MODULE CHARACTER(len=20) FUNCTION euler0025()
       USE euler_utility, ONLY: carry
       IMPLICIT NONE
       INTEGER(li), PARAMETER :: n = 1000_li
       INTEGER(li), DIMENSION(n) :: int_arr1, int_arr2, temp
       INTEGER(li) :: j, k
     END FUNCTION euler0025
     !
     MODULE CHARACTER(len=20) FUNCTION euler0026()
       USE euler_utility, ONLY: sieve_of_Eratosthenes
       IMPLICIT NONE
       INTEGER(ll) :: temp, k, ki, i
       LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
     END FUNCTION euler0026
     !
     MODULE CHARACTER(len=20) FUNCTION euler0027()
       IMPLICIT NONE
       INTEGER(li) :: a, b
     END FUNCTION euler0027
     !
     MODULE CHARACTER(len=20) FUNCTION euler0028()
       IMPLICIT NONE
       INTEGER(li) :: i, k, ur, bl, br, ul, temp
     END FUNCTION euler0028
     !
     MODULE CHARACTER(len=20) FUNCTION euler0029()
       IMPLICIT NONE
       INTEGER, PARAMETER :: lim = 99
       INTEGER(li), DIMENSION(2, lim, lim) :: array_of_nums
       INTEGER(li), DIMENSION(2, lim*lim) :: arr, res
       INTEGER(li) :: i, j, k
     END FUNCTION euler0029
     !
     MODULE CHARACTER(len=20) FUNCTION euler0030()
       USE euler_utility, ONLY: int_2_arr
       IMPLICIT NONE
       INTEGER(li) :: temp, i
       INTEGER(li), ALLOCATABLE, DIMENSION(:) :: arr
     END FUNCTION euler0030
     !
     MODULE CHARACTER(len=20) FUNCTION euler0031()
       IMPLICIT NONE
       INTEGER(li), DIMENSION(0:7) :: coins
       INTEGER(li), DIMENSION(0:7, 0:200) :: ans_arr
       INTEGER(li) :: i, j
     END FUNCTION euler0031
     !
     MODULE CHARACTER(len=20) FUNCTION euler0032()
       USE euler_utility, ONLY: length_of_int, is_pandigital, remove_duplicates
       IMPLICIT NONE
       INTEGER(ll) :: temp, i, j, k
       INTEGER(ll), DIMENSION(9) :: array_of_products
       INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: array_of_non_repeats
     END FUNCTION euler0032
     !
     MODULE CHARACTER(len=20) FUNCTION euler0033()
       USE euler_utility, ONLY: gcd
       IMPLICIT NONE
       INTEGER(li) :: c, d, n, dp, np
     END FUNCTION euler0033
     !
     MODULE CHARACTER(len=20) FUNCTION euler0034()
       IMPLICIT NONE
       INTEGER(li) :: i, sum
     END FUNCTION euler0034
     !
     MODULE CHARACTER(len=20) FUNCTION euler0035()
       IMPLICIT NONE
       INTEGER(li) :: i
       INTEGER(li), PARAMETER :: limit = 1000000_li
       LOGICAL, DIMENSION(limit) :: logical_array
       INTEGER(li), ALLOCATABLE, DIMENSION(:) :: array
     END FUNCTION euler0035
     !
     MODULE CHARACTER(len=20) FUNCTION euler0036()
       IMPLICIT NONE
       INTEGER(ll), PARAMETER :: limit = 1000000_ll
       INTEGER(ll) :: sum, i, next_ten_pow, p
     END FUNCTION euler0036
     !
     MODULE CHARACTER(len=20) FUNCTION euler0037()
       USE euler_utility, ONLY: is_prime, append, pop
       IMPLICIT NONE
       INTEGER(li) :: sum, cur, push_size, temp, i
       INTEGER(li), DIMENSION(4) :: push_array
       INTEGER(li), ALLOCATABLE, DIMENSION(:) :: pre_queue
     END FUNCTION euler0037
     !
     MODULE CHARACTER(len=20) FUNCTION euler0038()
       USE euler_utility, ONLY: is_pandigital, length_of_int, append
       IMPLICIT NONE
       INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: common_array
       INTEGER(ll) :: i, j, temp
     END FUNCTION euler0038
     !
     MODULE CHARACTER(len=20) FUNCTION euler0039()
       IMPLICIT NONE
       INTEGER(li) :: i, j, k
       INTEGER(li), DIMENSION(1000) :: count_array
     END FUNCTION euler0039
     !
     MODULE CHARACTER(len=20) FUNCTION euler0040()
       USE euler_utility, ONLY: append, int_2_arr
       IMPLICIT NONE
       INTEGER(li) :: i, k
       INTEGER(li), ALLOCATABLE, DIMENSION(:) :: array, int_array
     END FUNCTION euler0040
     !
     MODULE CHARACTER(len=20) FUNCTION euler0041()
       USE euler_utility, ONLY: is_prime, is_pandigital, length_of_int
       IMPLICIT NONE
       INTEGER(li) :: i
     END FUNCTION euler0041
     !
     MODULE CHARACTER(len=20) FUNCTION euler0042()
       IMPLICIT NONE
       CHARACTER(len=20) :: file_name
       CHARACTER(len=20), DIMENSION(2000) :: names
       INTEGER(li) :: stat, i, j, temp
     END FUNCTION euler0042
     !
     MODULE CHARACTER(len=20) FUNCTION euler0043()
       IMPLICIT NONE
       INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: test_array
     END FUNCTION euler0043
     !
     MODULE CHARACTER(len=20) FUNCTION euler0044()
       IMPLICIT NONE
       INTEGER(ll) :: i, j, minimised_D
       INTEGER(ll) :: Pm, Pn
     END FUNCTION euler0044
     !
     MODULE CHARACTER(len=20) FUNCTION euler0045()
       IMPLICIT NONE
       INTEGER(ll) :: n, hexagonal_number
     END FUNCTION euler0045
     !
     MODULE CHARACTER(len=20) FUNCTION euler0046()
       USE euler_utility, ONLY: sieve_of_Eratosthenes
       IMPLICIT NONE
       INTEGER(li), PARAMETER :: n = 10000_li
       INTEGER(li) :: j, res = 1_li
       LOGICAL :: not_found = .TRUE.
       LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
     END FUNCTION euler0046
     !
     MODULE CHARACTER(len=20) FUNCTION euler0047()
       IMPLICIT NONE
       INTEGER(li), PARAMETER :: maxx = 1000000_li
       INTEGER(li), DIMENSION(maxx) :: n_factor
       INTEGER(li), DIMENSION(4) :: goal
       INTEGER(li) :: i, j
     END FUNCTION euler0047
     !
     MODULE CHARACTER(len=20) FUNCTION euler0048()
       IMPLICIT NONE
       INTEGER(li), DIMENSION(10) :: sum
       INTEGER(li) :: i
     END FUNCTION euler0048
     !
     MODULE CHARACTER(len=20) FUNCTION euler0049()
       USE euler_utility, ONLY: sieve_of_Eratosthenes
       IMPLICIT NONE
       INTEGER(li) :: i, a, b, c
       CHARACTER(4) :: str1, str2, str3
       LOGICAL :: cond1, cond2
       LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
     END FUNCTION euler0049
     !
     MODULE CHARACTER(len=20) FUNCTION euler0050()
       INTEGER(ll), PARAMETER :: maxx = 1000000_ll
       INTEGER(ll) :: n, i, j, cur_sum
       INTEGER(ll), ALLOCATABLE, DIMENSION(:) :: p_arr
     END FUNCTION euler0050
     !
     MODULE CHARACTER(len=20) FUNCTION euler0051()
       USE euler_utility, ONLY: li, sieve_of_Eratosthenes, unit_digit
       IMPLICIT NONE
       INTEGER(li) :: n, i, j, k, test_int
       INTEGER(li) :: u, u_min, u_max, v
       LOGICAL, ALLOCATABLE, DIMENSION(:) :: is_prime
       INTEGER(li), ALLOCATABLE, DIMENSION(:) :: int_arr, index_arr
     END FUNCTION euler0051
     !
     MODULE CHARACTER(len=20) FUNCTION euler0052()
      IMPLICIT NONE
      INTEGER(li) :: start = 1, ans = 0, i, j
      LOGICAL :: found = .FALSE.
     END FUNCTION euler0052 
  END INTERFACE
CONTAINS
  SUBROUTINE euler_check()
    IMPLICIT NONE
    ERROR STOP "EULER_CHECK: This function shouldn't be called anywhere."
  END SUBROUTINE euler_check
END MODULE euler_problems

submodule(euler_interface_m) euler_prob_0051_m
    implicit none

    !
    ! The function next_permutation is very necessary, some answer online might
    ! just use a manual list to replace it. The result is fast but it is kinda
    ! cheating cuz the problem says "by replacing part of the number", so you do
    ! not know which digits are replaced. The only way to figure out is to list
    ! all the possibilities, which is permutation.
    !
    ! Some further optimizations could be done for this problem. One could
    ! construct a logical function called next_logic_permute and output the next
    ! permute by boolean variables, which should save a lot of memory. I have 
    ! not figure out how to best implement that yet.
    !---------------------------------------------------------------------------

contains

    module character(len=20) function euler0051()
        euler0051 = '                   x'
        !write (euler0051, "(i20)") ans()
    end function euler0051

    ! integer function ans()
    !     use euler_primes_m, only: sieve_of_Eratosthenes
    !     implicit none

    !     integer :: n, i, j, k, test_int
    !     integer :: u, u_min, u_max, v
    !     logical, allocatable :: is_prime(:)
    !     integer, allocatable :: int_arr(:), index_arr(:)
    !     integer, dimension(6) :: not_prime

    !     not_prime = [0, 2, 4, 5, 6, 8]
    !     n = 999999

    !     call sieve_of_Eratosthenes(n, is_prime)
    !     index_arr = [0, 0, 0, 0, 0, 0]

    !     outermost: do v = 6, 1, -1
    !         index_arr(v:) = 1

    !         if ( allocated(int_arr) ) then 
    !             deallocate (int_arr)
    !         end if 

    !         allocate ( int_arr(size(index_arr) ) )

    !         n = count(index_arr == 0, dim=1)
    !         u_min = 10**(n - 1)
    !         u_max = 10**n - 1

    !         outer: do while ( next_permutation(index_arr) )
    !             inner: do u = u_min, u_max

    !                 if (                                                       &
    !                     any( not_prime == unit_digit(u) ) .and.                &
    !                     index_arr( size(index_arr) ) == 0                      &
    !                 ) then 
    !                     cycle inner
    !                 end if 

    !                 int_arr = 0
    !                 k = u
                    
    !                 do i = size(index_arr), 1, -1
    !                     if (index_arr(i) == 0) then
    !                         int_arr(i) = unit_digit(k)
    !                         k = k/10
    !                     end if
    !                 end do

    !                 j = 0

    !                 innermost: do i = 0, 9
    !                     call arr_2_int(                                        &
    !                         int_arr + index_arr * i,                           &
    !                         test_int                                           &
    !                     )

    !                     if ( .not. is_prime(test_int) ) then 
    !                         j = j + 1
    !                     end if 

    !                     if (j == 3) then
    !                         exit innermost
    !                     else if (i == 9) then
    !                         exit outermost
    !                     end if
    !                 end do innermost

    !                 end do inner
    !             end do outer
    !         end do outermost

    !     do i = size(index_arr), 1, -1
    !         if (index_arr(i) == 0) then
    !             index_arr(i) = unit_digit(u)
    !             u = u/10
    !         end if
    !     end do

    !     call arr_2_int(index_arr, ans)
    ! end function ans

    ! logical function next_permutation(input_arr)
    !     integer, dimension(:), intent(inout) :: input_arr
    !     integer :: i, j

    !     next_permutation = .false.
    !     i = size(input_arr)

    !     do while (                                                             &
    !         i > 1 .and.                                                        &
    !         input_arr(i - 1) >= input_arr(i)                                   &
    !     )
    !         i = i - 1
    !     end do

    !     if (i <= 1) then
    !         next_permutation = .false.
    !         return
    !     end if

    !     j = size(input_arr)
        
    !     do while (                                                             &
    !         input_arr(j) <= input_arr(i - 1)                                   &
    !     )
    !         j = j - 1
    !     end do
        
    !     call swap(                                                             &
    !         input_arr(i - 1),                                                  &
    !         input_arr(j)                                                       &
    !     )

    !     input_arr(i:) = input_arr(                                             &
    !         size(input_arr):i:-1                                               &
    !     )

    !     next_permutation = .true.
    ! end function next_permutation

end submodule euler_prob_0051_m
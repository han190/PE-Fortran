submodule(euler_interface_m) euler_problem_0058
    implicit none 

contains 

    module character(len=20) function euler0058()
        write (euler0058, "(i20)") 0!ans()
    end function euler0058 

    ! The problem is solved but it is really slow
    ! I am writing a Miller-Rabin test

    ! integer function ans()
    !     use euler_primes_m, only: sieve_of_Eratosthenes
    !     implicit none 
    !     logical, allocatable :: is_prime(:)
    !     integer(int64) :: n, k, arr(4)
    !     integer(int64), parameter :: rng = 700000000_int64

    !     call sieve_of_Eratosthenes(rng, is_prime)
    !     k = 0_int64; n = 2_int64

    !     main_loop: do
    !         call diagonal_nums(n, arr)

    !         if ( arr(4) >= rng ) then 
    !             error stop "Max range reached."
    !         end if 
    !         ! since lower right corner is always (n-1)**2
    !         k = k + count( is_prime(arr(1:3)) )

    !         associate( &
    !             p_nums => real(k), &
    !             tot_nums => real( (n - 1_int64) * 4_int64 + 1_int64 ) &
    !         )
    !             if (p_nums / tot_nums < 0.1_sp) then
    !                 exit main_loop
    !             end if 
    !         end associate

    !        n = n + 1_int64
    !     end do main_loop

    !     ans = side_len(n)

    ! end function ans

    ! subroutine diagonal_nums(n, arr)
    !     integer(int64), intent(in) :: n 
    !     integer(int64) :: a, b, c, d
    !     integer(int64), intent(out) :: arr(4)

    !     d = ( n * 2_int64 - 1_int64 ) ** 2_int64 
    !     associate( x => (n - 1_int64) * 2_int64 )
    !         c = d - x 
    !         b = c - x 
    !         a = b - x
    !     end associate

    !     arr = [a, b, c, d]
    ! end subroutine diagonal_nums

    ! integer function side_len(n)
    !     integer(int64), intent(in) :: n 

    !     side_len = n * 2_int64 - 1_int64
    ! end function side_len

end submodule euler_problem_0058

submodule(euler_interface_m) euler_prob_0055_m
    use euler_mi_m
    implicit none
    !
    ! OK, I wrote an arbitrary precision library for this problem using the
    ! oo features of Fortran2003. No doubt it is slower than directly storing
    ! all the big numbers into allocatable integer(1) arrays but writing
    ! everything into oo structures makes my code cleaner and the lib could be
    ! reused if I want to.
    !---------------------------------------------------------------------------

contains

    module character(len=20) function euler0055()
        write (euler0055, "(i20)") ans(10000)
    end function euler0055

    integer function ans(upper_bound)
        integer, intent(in) :: upper_bound
        type(very_long_int_t) :: num
        integer :: i, k

        k = 0

        do i = 1, upper_bound
            num = i

            if (is_lychrel(num)) then
                k = k + 1
            end if
        end do

        ans = k
    end function ans

    logical function is_lychrel(num)
        type(very_long_int_t), intent(in) :: num
        type(very_long_int_t) :: tmp, tmp2
        integer :: i

        i = 0
        is_lychrel = .true.
        tmp = num

        do while (i <= 50)
            tmp2 = reverse_long(tmp) + tmp

            if (is_palindromic_long(tmp2)) then
                is_lychrel = .false.
                return
            end if

            tmp = tmp2
            i = i + 1
        end do
    end function is_lychrel

    logical function is_palindromic_long(i)
        type(very_long_int_t), intent(in) :: i
        type(very_long_int_t) :: tmp

        tmp = reverse_long(i)
        if (tmp == i) then
            is_palindromic_long = .true.
        else
            is_palindromic_long = .false.
        end if
    end function is_palindromic_long

    function reverse_long(i) result(a)
        type(very_long_int_t), intent(in) :: i
        type(very_long_int_t) :: a

        associate (x => size(i%arr))
            allocate (a%arr(x))
            a%arr(1:x) = i%arr(x:1:-1)
            a%sgn = '+'
        end associate
    end function reverse_long

end submodule euler_prob_0055_m

submodule(interface_m) euler_problem_0049_m
    implicit none

contains

    module character(len=20) function euler0049()
        write (euler0049, "(a20)") trim(answer())
    end function euler0049

    pure character(len=12) function answer()
        use prime_m, only: Sieve_of_Eratosthenes
        implicit none

        integer(i32), parameter :: n = 10000
        integer(i32) :: i
        logical, allocatable :: is_prime(:)

        allocate (is_prime(n))
        call Sieve_of_Eratosthenes(n, is_prime)
        do i = 9973, 7661, -1
            associate (a => i, b => i - 3330, c => i - 6660)
                if (.not. all([is_prime(a), is_prime(b), is_prime(c)])) cycle
                if (sort(a) == sort(b) .and. sort(a) == sort(c)) exit
            end associate
        end do

        write (answer, "(3(i4))") i - 6660, i - 3330, i
    end function answer

    pure integer(i32) function sort(n)
        integer(i32), intent(in) :: n
        integer(i32) :: i, j
        integer(i32), allocatable :: array(:)

        array = to_array(n)
        do i = 1, 4
            do j = i + 1, 4
                if (array(i) > array(j)) call swap(array(i), array(j))
            end do
        end do
        sort = to_integer(array)
    end function sort

    pure subroutine swap(a, b)
        integer(i32), intent(inout) :: a, b
        integer(i32) :: temp

        temp = a; a = b; b = temp
    end subroutine swap

end submodule euler_problem_0049_m

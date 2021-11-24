submodule(interface_m) euler_problem_0002_m
    implicit none

contains

    module pure character(len=20) function euler0002()
        write (euler0002, "(i20)") answer()
    end function euler0002

    pure integer(i32) function answer()
        integer(i32), parameter :: n = 4000000
        integer(i32) :: i

        i = 0; answer = 0
        do while (fibonacci(i) <= n)
            answer = answer + fibonacci(i)
            i = i + 3
        end do
    end function answer

    pure recursive function fibonacci(n) result(ret)
        integer(i32), intent(in) :: n
        integer(i32) :: k, ret

        if (n == 0) then
            ret = 0
        else if (any(n == [1, 2])) then
            ret = 1
        else if (mod(n, 2) == 0) then
            k = n/2
            ret = fibonacci(k)*(fibonacci(k + 1)*2 - fibonacci(k))
        else
            k = (n - 1)/2
            ret = fibonacci(k + 1)**2 + fibonacci(k)**2
        end if
    end function fibonacci

end submodule euler_problem_0002_m

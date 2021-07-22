submodule(euler_interface_m) euler_prob_0024_m
    implicit none

contains

    module character(len=20) function euler0024()
        write (euler0024, "(i20)") ans()
    end function euler0024

    integer(int64) function ans()
        integer(int64) :: factor_array(10)
        integer(int64), allocatable :: arr1(:), arr2(:)
        integer(int64) :: n, i, j, k, tmp

        factor_array = [1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880]
        allocate (arr1(10))
        arr1 = [(i, i=0, 9)]
        n = 999999
        tmp = 0

        do i = 10, 1, -1
            j = n/factor_array(i)
            n = mod(n, factor_array(i))
            k = arr1(j + 1)

            allocate (arr2(count(arr1 /= k)))
            arr2 = pack(arr1, arr1 /= k)
            deallocate (arr1)

            allocate (arr1(size(arr2)))
            arr1 = arr2
            deallocate (arr2)

            tmp = tmp + k*10**(i - 1)
        end do
        ans = tmp
    end function ans

end submodule euler_prob_0024_m

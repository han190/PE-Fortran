submodule(euler_interface_m) euler_prob_0024_m
    implicit none 

contains 

    module character(len=20) function euler0024()
        write (euler0024, "(i20)") ans()
    end function euler0024 

    integer(int64) function ans()
        integer(int64) :: factor_array(10)
        integer(int64), allocatable :: arr1(:), arr2(:)
        integer(int64) :: n, i, j, k, l, m, tmp

        factor_array = [                                                       &
            1_int64, 1_int64, 2_int64, 6_int64,                                &
            24_int64, 120_int64, 720_int64,                                    &
            5040_int64, 40320_int64, 362880_int64                              &
        ]

        allocate ( arr1(10) )
        arr1 = [ (i, i = 0_int64, 9_int64) ]
        n = 999999_int64; tmp = 0_int64

        do i = 10_int64, 1_int64, -1_int64
            j = n / factor_array(i)
            n = mod( n, factor_array(i) )

            k = arr1(j + 1_int64)

            !arr2 = pack(arr1, arr1 /= k)
            allocate( arr2( count(arr1 /= k) ) )
            m = 1
            do l = 1, size(arr1)
                if ( arr1(l) /= k ) then
                    arr2(m) = arr1(l)
                    m = m + 1
                end if
            end do
            deallocate (arr1)

            allocate ( arr1( size(arr2) ) )
            arr1 = arr2
            deallocate (arr2)

            tmp = tmp + k * 10_int64**(i - 1_int64)
        end do
        ans = tmp 
    end function ans 


end submodule euler_prob_0024_m

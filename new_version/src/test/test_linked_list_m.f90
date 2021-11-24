program main

    use linked_list_m
    use constant_m
    implicit none

    type(linked_list_t) :: list
    integer(i32) :: i
    integer(i32), allocatable :: arr(:)

    do i = 1, 1000000
        call list%append(i)
    end do 

    arr = list%to_array()
    ! print *, arr
end program main
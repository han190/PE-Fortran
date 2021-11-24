module linked_list_m

    use constant_m
    implicit none
    private

    !> Node type
    type node_t
        integer(i32) :: value
        type(node_t), allocatable :: next
    end type node_t

    !> Linked list
    type, public :: linked_list_t
        integer(i32) :: length = 0_i32 ! Follow Fortran convention.
        type(node_t), pointer :: head => null()
        type(node_t), pointer :: tail => null()
    contains
        procedure :: append => append_sub
        procedure :: at => at_func
        procedure :: to_array => to_array_func
    end type

contains

    !> Append to list
    pure subroutine append_sub(self, value)
        class(linked_list_t), intent(inout) :: self
        integer(i32), intent(in) :: value

        self%length = self%length + 1_i32

        if (.not. associated(self%head)) then
            allocate (self%head)
            self%tail => self%head
            self%tail%value = value
        else
            allocate (self%tail%next)
            self%tail => self%tail%next
            self%tail%value = value
        end if
    end subroutine append_sub

    !> 
    integer(i32) function at_func(self, index)
        class(linked_list_t), intent(in) :: self
        integer(i32), intent(in) :: index
        integer(i32) :: i
        type(node_t), pointer :: ptr

        if (index >= 1_i32 .and. index <= self%length) then
            ptr => self%head
            do i = 1, index - 1
                ptr => ptr%next
            end do
        else
            error stop "Index out of range."
        end if

        at_func = ptr%value
    end function at_func

    !> Convert linked list to an allocatable array.
    function to_array_func(self) result(ret)
        class(linked_list_t), intent(in) :: self
        integer(i32), allocatable :: ret(:)
        integer(i32) :: i
        type(node_t), pointer :: ptr

        allocate (ret(self%length))
        ptr => self%head
        ret(1) = ptr%value
        do i = 2, self%length
            ptr => ptr%next
            ret(i) = ptr%value
        end do
    end function to_array_func

end module linked_list_m

module euler_problems_m

    use interface_m
    implicit none

    type :: euler_problem_t
        procedure(euler_problem_x), nopass, pointer :: answer => null()
    end type euler_problem_t

    abstract interface
        pure character(len=20) function euler_problem_x()
        end function euler_problem_x
    end interface

contains
    
    subroutine initialize_problems(problems)
        type(euler_problem_t), allocatable, intent(inout) :: problems(:)

        allocate(problems(16))
        problems(1)%answer => euler0001
        problems(2)%answer => euler0002
        problems(3)%answer => euler0003
        problems(4)%answer => euler0004
        problems(5)%answer => euler0005
        problems(6)%answer => euler0006
        problems(7)%answer => euler0007
        problems(8)%answer => euler0008
        problems(9)%answer => euler0009
        problems(10)%answer => euler0010
        problems(11)%answer => euler0011
        problems(12)%answer => euler0012
        problems(13)%answer => euler0013
        problems(14)%answer => euler0014
        problems(15)%answer => euler0015
        problems(16)%answer => euler0016
    end subroutine initialize_problems

end module euler_problems_m
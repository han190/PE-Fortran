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

        allocate (problems(${NUM_PROB}$))
        #: for i in range(1, NUM_PROB + 1)
        problems(${i}$)%answer => euler${"%4.4d" % (i,)}$
        #: endfor
    end subroutine initialize_problems

end module euler_problems_m

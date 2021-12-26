module problems_m

    use interface_m
    implicit none

    type :: problem_t
        procedure(problem_x), nopass, pointer :: answer => null()
    end type problem_t

    abstract interface
        character(len=20) function problem_x()
        end function problem_x
    end interface

contains

    subroutine initialize_problems(problems)
        type(problem_t), allocatable, intent(inout) :: problems(:)

        allocate (problems(${NUM_PROB}$))
        #: for i in range(1, NUM_PROB + 1)
        problems(${i}$)%answer => euler${"%4.4d" % (i,)}$
        #: endfor
    end subroutine initialize_problems

end module problems_m
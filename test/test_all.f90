program main
use module_problem
use module_interface
implicit none

character(*), parameter :: data_dir = "./data/"
character(*), parameter :: answer = "answer.log"
integer, parameter :: num_trails = 1
type(problem_type), allocatable :: problems(:)
type(solution_type), allocatable :: solutions(:)

call new_solutions(solutions)
call new_problems(problems, data_dir)
call solve_problems(problems, solutions, num_trails)
call answer_sheet(problems, answer)
end program main

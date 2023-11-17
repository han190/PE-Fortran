program main

use :: iso_fortran_env, only: int64
use :: module_problem
implicit none

character(*), parameter :: data_dir = "data"
character(*), parameter :: answer = "answer.log"
integer(int64), parameter :: num_trails = 1
type(problem_type), allocatable :: problems(:)

call new_problems(problems, data_dir)
call solve_problems(problems, num_trails)
call answer_sheet(problems, answer)

end program main

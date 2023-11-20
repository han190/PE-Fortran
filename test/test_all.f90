program main

use :: iso_fortran_env, only: int64
use :: module_problem
implicit none

type(problem_type), allocatable :: problems(:)
integer(int64) :: num_trails = 1_int64
problems = new_problems("data")
call solve_problems(problems, num_trails)
call print_answers(problems, "answer.log")

end program main

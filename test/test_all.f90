program main

use :: iso_fortran_env, only: int64
use :: module_problem
implicit none

type(problem_type), allocatable :: problems(:)
problems = new_problems("data")
call solve_problems(problems, 1_int64)
call print_answers(problems, "answer.log")

end program main

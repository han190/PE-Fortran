module module_interface

use :: euler_toolkit
use :: module_problem
implicit none

public :: new_solutions
private 

!> Problem interface
interface
  include "interface.inc"
end interface

contains

!> Construct new solutions
subroutine new_solutions(solutions)
  type(solution_type), allocatable, intent(inout) :: solutions(:)
  include "solutions.inc"
end subroutine new_solutions

end module module_interface
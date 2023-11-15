submodule(module_interface) submodule_euler0025
implicit none
contains

module subroutine euler0025(problem)
  type(problem_type), intent(inout) :: problem
  real(real64) :: phi
  integer(int32) :: sln

  !> Fn = round(phi**n/sqrt(5)), Solve for num_digits(Fn) = 1000
  phi = 0.5*(1.0 + sqrt(5.0_real64))
  sln = ceiling((1000 - 1 + 0.5*log10(5.0))/log10(phi))
  write (problem%answer, "(i20)") sln
end subroutine euler0025

end submodule submodule_euler0025

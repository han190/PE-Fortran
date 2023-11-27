submodule(module_problem) submodule_euler0027
!> Take the general form of Euler primes (n - x)**2 + (n - x) + 41, rewrite it
!> as n**2 - (2*x - 1)*n + (x**2 - x + 41). Since |a| < 1000 and |b| <= 1000,
!> we have |2*x - 1| < 1000 and |x**2 - x + 41| <= 1000. Solve the second
!> equation for x, we get x = 0.5*(1 +/- sqrt(3837)), or -30 <= x <= 31. 
!> Reference: https://mathworld.wolfram.com/Prime-GeneratingPolynomial.html
implicit none
contains

module subroutine euler0027(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: x = 31

  write (problem%answer, "(i20)") -(2*x - 1)*(x**2 - x + 41)
end subroutine euler0027

end submodule submodule_euler0027
submodule(module_interface) submodule_euler0033
implicit none
contains

module subroutine euler0033(problem)
  type(problem_type), intent(inout) :: problem
  integer(int32) :: denom, numer, counter, denom_prod, numer_prod

  denom = 1
  numer = 1
  denom_prod = 1
  numer_prod = 1

  do counter = 1, 9
    do denom = 1, counter - 1
      do numer = 1, denom - 1
        if ((numer*10 + counter)*denom == (counter*10 + denom)*numer) then
          denom_prod = denom_prod*denom
          numer_prod = numer_prod*numer
        end if
      end do
    end do
  end do
  write (problem%answer, "(i20)") denom_prod/gcd(numer_prod, denom_prod)
end subroutine euler0033

end submodule submodule_euler0033

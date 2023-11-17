submodule(module_interface) submodule_euler0046
implicit none
contains

module subroutine euler0046(problem)
  type(problem_type), intent(inout) :: problem
  type(sieve_type(len=:)), allocatable :: sieve
  integer(int64) :: i, sln
  logical :: not_found
  logical, pointer :: check(:) => null()

  allocate (sieve_type(len=10000) :: sieve)
  call sift(sieve, check=check)
  sln = 1
  not_found = .true.

  outer: do while (not_found)
    sln = sln + 2
    i = 2
    not_found = .false.

    inner: do while (sln >= i)
      if (check(i) .and. is_twice_square(sln - i)) then
        not_found = .true.
        exit inner
      end if
      i = i + 1
    end do inner
  end do outer
  write (problem%answer, "(i20)") sln
  nullify (check)
end subroutine euler0046

elemental logical function is_twice_square(n)
  integer(int64), intent(in) :: n
  real(real64) :: sqrt_nover2

  is_twice_square = .false.
  sqrt_nover2 = sqrt(0.5_real64*real(n, real64))

  if (sqrt_nover2 - real(floor(sqrt_nover2), real64) < tiny(0.0_real64)) &
    is_twice_square = .true.
end function is_twice_square

end submodule submodule_euler0046

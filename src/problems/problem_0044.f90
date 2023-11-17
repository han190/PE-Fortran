submodule(module_interface) submodule_euler0044
implicit none
contains

module subroutine euler0044(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 3000
  integer(int64) :: pentagonals(n)
  integer(int64) :: i, j, sln

  i = 1
  j = 1
  do while (i <= n)
    if (is_pentagonal(j)) then
      pentagonals(i) = j
      i = i + 1
    end if
    j = j + 1
  end do

  sln = huge(0_int64)
  do i = 1, n
    do j = i + 1, n
      associate (pm => pentagonals(i), pn => pentagonals(j))
        if (is_pentagonal_pair(pm, pn)) sln = min(sln, pn - pm)
      end associate
    end do
  end do
  write (problem%answer, "(i20)") sln
end subroutine euler0044

pure logical function is_pentagonal_pair(pj, pk)
  integer(int64), intent(in) :: pj, pk

  associate (s => pj + pk, d => pk - pj)
    is_pentagonal_pair = is_pentagonal(s) .and. is_pentagonal(d)
  end associate
end function is_pentagonal_pair

elemental logical function is_pentagonal(p)
  integer(int64), intent(in) :: p

  associate (x => sqrt(24.0*real(p) + 1.0_real64))
    is_pentagonal = is_integer(x) .and. mod(int(x, int64), 6_int64) == 5_int64
  end associate
end function is_pentagonal

elemental logical function is_integer(n)
  real(real64), intent(in) :: n

  is_integer = n - floor(n) <= tiny(0.0_real64)
end function is_integer

end submodule submodule_euler0044

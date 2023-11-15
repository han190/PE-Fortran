program main
use :: module_permutation
implicit none

type(permutation_type(n=10, k=7)) :: permutation
integer, allocatable :: indices(:), cycles(:)
logical :: passed

call initialize(permutation)
do while (permutable(permutation))
  indices = permutation%indices
  cycles = permutation%cycles
  call initialize(permutation, start=indices)
  passed = all(permutation%indices == indices) .and. &
    & all(permutation%cycles == cycles)
  if (.not. passed) then
    print "(a)", "[Test not passed!]"
    stop
  end if
  call permute(permutation)
end do
end program main
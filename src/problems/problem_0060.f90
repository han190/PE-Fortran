submodule(module_interface) submodule_euler0060
implicit none
contains

module subroutine euler0060(problem)
  type(problem_type), intent(inout) :: problem
  integer(int64), parameter :: num_primes = 5000000
  integer(int64), parameter :: num_concats = 1500
  logical, allocatable :: prime_check(:), concat_check(:, :)
  integer(int64), allocatable :: primes(:)
  integer(int64), allocatable :: indices(:)
  type(permutation_type(n=:, k=:)), allocatable :: permutation
  integer(int64) :: i, k, pivot(2)
  logical :: checked, succeed

  ! allocate (prime_check(num_primes))
  ! call Sieve_of_Eratosthenes(prime_check)
  ! k = 1
  ! allocate (primes(count(prime_check)))
  ! do i = 1, count(prime_check)
  !   if (prime_check(i)) then
  !     primes(k) = i
  !     k = k + 1
  !   end if
  ! end do
  ! if (num_concats > size(primes)) &
  !   & error stop "[P60] num_concats > size(primes)"

  ! k = 4
  ! indices = [(i, i=2, 1 + k)]
  ! pivot = [1, 2]

  ! permutation = new_permutation(int(num_concats, int64), int(k, int64))
  ! call set_permutation(permutation, indices)

  ! if (.not. allocated(concat_check)) then
  !   allocate (concat_check(num_concats, num_concats))
  !   concat_check = .true.
  !   concat_check(:, 1) = .false.
  !   concat_check(1, :) = .false.
  ! end if

  ! do while (permutable(permutation))
  !   if (.not. checked_before(permutation, concat_check, pivot)) then
  !     succeed = prime_set(permutation, primes, prime_check, concat_check, pivot)
  !     if (succeed) exit
  !   end if
  !   call fast_permute(permutation, pivot)
  ! end do
  ! print *, primes(permutation%indices)
  ! indices = permutation%indices
  write (problem%answer, *) ""
end subroutine euler0060

! function checked_before(permutation, concat_check, pivot) result(checked)
!   type(permutation_type), intent(in) :: permutation
!   logical, intent(inout) :: concat_check(:, :)
!   integer(int64), intent(inout) :: pivot(:) !> size = 2
!   logical :: checked
!   type(permutation_type) :: inner_permutation

!   inner_permutation = new_permutation(permutation%k, 2)
!   checked = .false.
!   do while (permutable(inner_permutation))
!     associate (indices => permutation%indices(inner_permutation%indices))
!       if (.not. concat_check(indices(1), indices(2)) .and. &
!         & all(indices < size(concat_check, 1))) then
!         checked = .true.
!         pivot = indices
!         return
!       end if
!     end associate
!     call permute(inner_permutation)
!   end do
! end function checked_before

! subroutine fast_permute(permutation, pivot)
!   type(permutation_type), intent(inout) :: permutation
!   integer(int64), intent(in) :: pivot(:)
!   integer(int64), allocatable :: new_indices(:)
!   integer(int64) :: i

!   associate (p => maxval(pivot), k => permutation%k, &
!     & indices => permutation%indices)
!     ! if (p /= 0 .and. p /= k .and. indices(k) /= permutation%n) then
!       new_indices = indices
!       new_indices(p:) = [(int(indices(p) + i, int64), i=1, k - p + 1)]
!       call set_permutation(permutation, new_indices)
!     ! else
!     !   call permute(permutation)
!     ! end if
!   end associate
! end subroutine fast_permute

! logical function prime_set(permutation, primes, prime_check, concat_check, pivot)
!   type(permutation_type), intent(in) :: permutation
!   integer(int64), intent(in) :: primes(:)
!   logical, intent(in) :: prime_check(:)
!   logical, intent(inout) :: concat_check(:, :)
!   integer(int64), intent(inout) :: pivot(:) !> size = 2
!   integer(int64), allocatable :: sel_primes(:)
!   type(permutation_type) :: inner_permutation
!   integer(int64) :: i, ploc(2), pair(2)

!   sel_primes = primes(permutation%indices)
!   inner_permutation = new_permutation(size(sel_primes), 2)
!   prime_set = .true.
!   pivot = 0
!   do while (permutable(inner_permutation))
!     pair = sel_primes(inner_permutation%indices)
!     if (.not. prime_pair(pair, prime_check)) then
!       prime_set = .false.
!       pivot = inner_permutation%indices
!       exit
!     end if
!     call permute(inner_permutation)
!   end do

!   if (all(pivot /= 0)) then
!     ploc = [(permutation%indices(pivot(i)), i=1, 2)]
!     if (all(ploc < size(concat_check, 1)) .and. &
!       & concat_check(ploc(1), ploc(2))) then
!       concat_check(ploc(1), ploc(2)) = .false.
!       concat_check(ploc(2), ploc(1)) = .false.
!     end if
!   end if
! end function prime_set

! logical function prime_pair(primes, prime_check)
!   integer(int64), intent(in) :: primes(:) !> size = 2
!   logical, intent(in) :: prime_check(:)
!   integer(int64) :: i, array(2)

!   array = [concat(primes(1), primes(2)), concat(primes(2), primes(1))]
!   prime_pair = .true.
!   do i = 1, 2
!     if (array(i) > size(prime_check)) then
!       prime_pair = is_prime(array(i))
!     else
!       prime_pair = prime_check(array(i))
!     end if
!     if (.not. prime_pair) exit
!   end do
! end function prime_pair

! pure integer(int64) function concat(a, b)
!   integer(int64), intent(in) :: a, b

!   concat = a*10**num_digits(b) + b
! end function concat

end submodule submodule_euler0060

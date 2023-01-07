submodule(interface_module) euler_problem_0037_submodule
implicit none

contains

module character(len=20) function euler0037()
  write (euler0037, "(i20)") answer()
end function euler0037

pure integer(i64) function answer()
  use prime_module, only: Sieve_of_Eratosthenes
  implicit none

  integer(i64), parameter :: n = 1000000_i64
  integer(i64) :: i, knt
  logical, allocatable :: is_prime(:)

  allocate (is_prime(n))
  call Sieve_of_Eratosthenes(n, is_prime)
  i = 10; knt = 0; answer = 0

  do
    if (knt == 11 .or. i == n) exit
    if (is_prime(i) .and. is_trunc(i, is_prime)) then
      answer = answer + i
      knt = knt + 1
    end if
    i = i + 1
  end do
end function answer

pure logical function is_trunc(n, is_prime)
  integer(i64), intent(in) :: n
  logical, intent(in) :: is_prime(:)

  is_trunc = is_left_trunc(n, is_prime) .and. is_right_trunc(n, is_prime)
end function is_trunc

pure logical function is_left_trunc(n, is_prime)
  integer(i64), intent(in) :: n
  logical, intent(in) :: is_prime(:)
  integer(i64) :: tmp

  tmp = 10_i64
  do while (tmp < n .and. mod(n, tmp) >= 1)
    if (.not. is_prime(mod(n, tmp))) then
      is_left_trunc = .false.
      return
    end if
    tmp = tmp*10_i64
  end do
  is_left_trunc = .true.
end function is_left_trunc

pure logical function is_right_trunc(n, is_prime)
  integer(i64), intent(in) :: n
  logical, intent(in) :: is_prime(:)
  integer(i64) :: tmp

  tmp = n
  do while (tmp > 0_i64)
    if (is_prime(tmp)) then
      tmp = tmp/10_i64
    else
      is_right_trunc = .false.
      return
    end if
  end do
  is_right_trunc = .true.
end function is_right_trunc

end submodule euler_problem_0037_submodule

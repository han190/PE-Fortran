submodule(module_problem) submodule_euler0059
implicit none
contains

module subroutine euler0059(problem)
  class(problem_type), intent(inout) :: problem
  integer(int64), parameter :: n = 26, k = 3
  integer(int64), allocatable :: encrypted(:), decrypted(:)
  integer(int64) :: letters(n), i
  type(permutation_type) :: permutation
  integer(int64) :: unit, iostat

  allocate (encrypted(2000))
  encrypted = -1
  open (newunit=unit, file=problem%file, action="read", status="old")
  read (unit, *, iostat=iostat) encrypted
  close (unit)
  encrypted = pack(encrypted, encrypted /= -1)

  letters = [(i, i=97, 122)]
  allocate (decrypted(size(encrypted)))

  permutation = new_permutation(n=n, k=k)
  do while (permutable(permutation))
    associate (key => (letters(index(permutation))))
      call decrypt(encrypted, key, decrypted)
    end associate
    if (is_english(decrypted)) exit
  end do
  write (problem%answer, "(i20)") sum(decrypted)
end subroutine euler0059

pure subroutine decrypt(encrypted, key, decrypted)
  integer(int64), intent(in) :: encrypted(:), key(:)
  integer(int64), intent(out) :: decrypted(:)
  integer(int64) :: i, k

  do i = 1, size(encrypted)
    k = mod(i, 3)
    if (k == 0) k = 3
    decrypted(i) = ieor(encrypted(i), key(k))
  end do
end subroutine decrypt

pure function is_english(decrypted) result(ret)
  integer(int64), intent(in) :: decrypted(:)
  logical :: ret
  character(len=1) :: check_(8)
  integer(int64) :: i, knt(8)
  logical :: english_features

  check_ = ["e", "t", "a", "o", "i", "n", "s", "h"]
  knt = [(count(decrypted == iachar(check_(i))), i=1, size(check_))]
  ret = .false.
  english_features = sum(knt)/real(size(decrypted))*100. > 45.
  if (english_features .and. maxloc(knt, dim=1) == 1) ret = .true.
end function is_english

end submodule submodule_euler0059

submodule(interface_module) euler_problem_0042_submodule
  implicit none

contains

  module character(len=20) function euler0042()
    write (euler0042, "(i20)") answer()
  end function euler0042

  integer(i32) function answer()
    integer(i32), parameter :: score_max = 26*20
    logical :: is_triangular(score_max)
    character(len=:), allocatable :: names(:)
    integer(i32) :: iunit, istat, i

    open (newunit=iunit, file=data_path//"/"//"data_0042.txt", &
          action="read", status="old")
    allocate (character(len=50) :: names(2000))
    names = "n/a"
    read (iunit, *, iostat=istat) names
    close (iunit)

    call triangular_number(score_max, is_triangular)
    answer = 0
    do i = 1, count(names /= "n/a")
      if (is_triangular(score(names(i)))) answer = answer + 1
    end do
  end function answer

  pure integer(i32) function score(str)
    character(len=*), intent(in) :: str
    integer(i32) :: i

    score = sum([(iachar(str(i:i)) - 64, i=1, len_trim(str))])
  end function score

  pure subroutine triangular_number(n, is_triangular)
    integer(i32), intent(in) :: n
    logical, intent(out) :: is_triangular(n)
    integer(i32) :: i

    is_triangular = .false.
    i = 1
    do while (i*(i + 1)/2 <= n)
      is_triangular(i*(i + 1)/2) = .true.
      i = i + 1
    end do
  end subroutine triangular_number

end submodule euler_problem_0042_submodule

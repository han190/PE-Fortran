submodule(interface_module) euler_problem_0022_submodule
  implicit none

contains

  module character(len=20) function euler0022()
    write (euler0022, "(i20)") answer()
  end function euler0022

  integer(i32) function answer()
    use stdlib_sorting, only: sort
    implicit none

    integer(i32) :: i, iunit, istat
    character(len=:), allocatable :: names(:)

    open (newunit=iunit, file=data_path//"/"//"data_0022.txt", &
          status="old", action="read")
    allocate (character(len=20) :: names(6000))
    names = "n/a"
    read (iunit, *, iostat=istat) names
    close (iunit)

    associate (x => count(names /= "n/a"))
      call sort(names(1:x))
      answer = sum([(i*score_of_letters(names(i)), i=1, x)])
    end associate
  end function answer

  pure integer function score_of_letters(str)
    character(*), intent(in) :: str
    integer(i32) :: i

    score_of_letters = sum([(iachar(str(i:i)) - 64, i=1, len_trim(str))])
  end function score_of_letters

end submodule euler_problem_0022_submodule

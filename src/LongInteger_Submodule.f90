submodule(long_integer_module) long_integer_submodule

  implicit none
  !> Inner function ends with a '_'.

contains

  !> Re-allocate a long_integer with length n.
  pure subroutine allocate_(val, n)
    type(long_integer), intent(inout) :: val
    integer(i32), intent(in) :: n

    if (allocated(val%digit)) then
      if (size(val%digit) /= n) then
        deallocate (val%digit)
        allocate (val%digit(n))
      end if
    else
      allocate (val%digit(n))
    end if
  end subroutine allocate_

  !> Cut leading zeros of an digit array.
  pure function cut_leading_zeros_(digit) result(cut)
    integer(i8), contiguous, intent(in) :: digit(:)
    integer(i8), allocatable :: cut(:)
    integer(i32) :: i

    do i = 1, size(digit)
      if (digit(i) /= 0_i8) then
        cut = digit(i:)
        return
      end if
    end do
  end function cut_leading_zeros_

  !> The function of carry.
  pure function carry_(digit, option) result(ret)
    integer(i8), contiguous, intent(in) :: digit(:)
    character, intent(in) :: option
    integer(i8), allocatable :: ret(:)
    integer(i8), allocatable :: tmp(:)
    integer(i32) :: i, n

    select case (option)
    case ('+')

      n = size(digit)
      ret = [[0_i8, 0_i8], digit]
      tmp = [(0_i8, i=1, n + 2)]

      do
        if (all(ret <= 9)) return
        tmp = ret - ret/10_i8*10_i8
        ret = tmp + [ret(2:)/10_i8, 0_i8]
      end do

    case ('-')

      n = size(digit)
      ret = digit
      tmp = [(0_i8, i=1, n)]

      do
        if (all(ret >= 0)) return
        where (ret < 0)
          tmp = 1_i8
          ret = ret + 10_i8
        end where
        tmp = cshift(tmp, 1)
        ret = ret - tmp
      end do

    end select
    error stop "Error: carry_."
  end function carry_

  !> Inner function to add two positive digit arrays.
  pure function add_(digit1, digit2) result(ret)
    integer(i8), contiguous, intent(in) :: digit1(:)
    integer(i8), contiguous, intent(in) :: digit2(:)
    integer(i8), allocatable :: ret(:)
    integer(i8), allocatable :: temp1(:), temp2(:)
    integer(i32) :: n, n1, n2

    n1 = size(digit1)
    n2 = size(digit2)
    n = max(n1, n2)
    allocate (temp1(n), temp2(n))

    associate (i1 => n - n1, i2 => n - n2)
      temp1(:i1) = 0; temp1(i1 + 1:) = digit1
      temp2(:i2) = 0; temp2(i2 + 1:) = digit2
    end associate
    ret = cut_leading_zeros_(carry_(temp1 + temp2, '+'))
  end function add_

  !> Inner function to subtract when digit2 >= digit1.
  pure function sub_(digit2, digit1) result(ret)
    integer(i8), contiguous, intent(in) :: digit2(:)
    integer(i8), contiguous, intent(in) :: digit1(:)
    integer(i8), allocatable :: ret(:)
    integer(i8), allocatable :: temp1(:), temp2(:)
    integer(i32) :: n1, n2

    n1 = size(digit1)
    n2 = size(digit2)
    allocate (temp1(n2), temp2(n2))
    temp2 = digit2
    temp1(:n2 - n1) = 0
    temp1(n2 - n1 + 1:) = digit1
    ret = cut_leading_zeros_(carry_(temp2 - temp1, '-'))
  end function sub_

  !> Inner function to compare two digit arrays.
  pure function comp_(digit1, digit2) result(ret)
    integer(i8), contiguous, intent(in) :: digit1(:)
    integer(i8), contiguous, intent(in) :: digit2(:)
    integer(i8) :: ret
    integer(i32) :: i, n1, n2

    n1 = size(digit1)
    n2 = size(digit2)

    if (n1 > n2) then
      ret = 1
      return
    else if (n1 < n2) then
      ret = -1
      return
    end if

    do i = n1, 1, -1
      if (digit1(i) > digit2(i)) then
        ret = 1
        return
      else if (digit1(i) < digit2(i)) then
        ret = -1
        return
      else
        ret = 0
      end if
    end do

    if (ret /= 0) then
      error stop 'Error: comp_'
    end if
  end function comp_

  !> Initialize a long_integer with an integer.
  pure module subroutine init_i32(val, int_)
    type(long_integer), intent(inout) :: val
    integer(i32), intent(in) :: int_
    integer(i32) :: temp, i
    integer(i32) :: num_digits

    val%sign = merge("+", "-", int_ >= 0)
    if (int_ == 0_i32) then
      val%digit = [0_i8]
      return
    end if

    temp = abs(int_)
    num_digits = floor(log10(real(temp))) + 1
    call allocate_(val, num_digits)
    do i = num_digits, 1, -1
      val%digit(i) = int(mod(temp, 10), i8)
      temp = temp/10
    end do
  end subroutine init_i32

  !> Initialize a long_integer with a character.
  pure module subroutine init_char(val, char_)
    type(long_integer), intent(inout) :: val
    character(len=*), intent(in) :: char_

    select case (char_(1:1))
    case ("+", "-")
      val%sign = char_(1:1)
      call allocate_(val, len(char_) - 1)
      read (char_(2:len(char_)), "(*(i1))") val%digit
    case default
      val%sign = "+"
      call allocate_(val, len(char_))
      read (char_(1:len(char_)), "(*(i1))") val%digit
    end select
  end subroutine init_char

  !> Initialize a long_integer with
  pure module subroutine init_i8_arr(val, digit_)
    type(long_integer), intent(inout) :: val
    integer(i8), intent(in) :: digit_(:)

    call allocate_(val, size(digit_))
    val%digit = digit_(:)
    val%sign = "+"
  end subroutine init_i8_arr

  !> Equal.
  pure module logical function eq(val1, val2)
    type(long_integer), intent(in) :: val1
    type(long_integer), intent(in) :: val2

    eq = all(val1%digit == val2%digit) &
         .and. val1%sign == val2%sign
  end function eq

  !> Greater than
  pure module logical function gt(val1, val2)
    type(long_integer), intent(in) :: val1
    type(long_integer), intent(in) :: val2
    logical :: pstv, ngtv
    integer(i8) :: c

    c = comp_(val1%digit, val2%digit)
    pstv = all([val1%sign, val2%sign] == "+")
    ngtv = all([val1%sign, val2%sign] == "-")
    gt = any([pstv .and. c == 1, ngtv .and. c == -1, &
              val1%sign == "+" .and. val2%sign == "-"])
  end function gt

  !> Less than
  pure module logical function lt(val1, val2)
    type(long_integer), intent(in) :: val1
    type(long_integer), intent(in) :: val2
    logical :: pstv, ngtv
    integer(i8) :: c

    c = comp_(val1%digit, val2%digit)
    pstv = all([val1%sign, val2%sign] == "+")
    ngtv = all([val1%sign, val2%sign] == "-")
    lt = any([pstv .and. c == -1, ngtv .and. c == 1, &
              val1%sign == "-" .and. val2%sign == "+"])
  end function lt

  !> Greater than or equal to.
  pure module logical function ge(val1, val2)
    type(long_integer), intent(in) :: val1
    type(long_integer), intent(in) :: val2

    ge = gt(val1, val2) .or. eq(val1, val2)
  end function ge

  !> Less than or equal to
  pure module logical function le(val1, val2)
    type(long_integer), intent(in) :: val1
    type(long_integer), intent(in) :: val2

    le = lt(val1, val2) .or. eq(val1, val2)
  end function le

  !> Add
  pure module function add(val1, val2) result(ret)
    type(long_integer), intent(in) :: val1, val2
    type(long_integer) :: ret
    integer(i8) :: c

    c = comp_(val1%digit, val2%digit)
    if (c == 0 .and. val1%sign /= val2%sign) then
      ret%digit = [0_i8]
      ret%sign = "+"
    else if (val1%sign == "+" .and. val2%sign == "+") then
      ret%digit = add_(val1%digit, val2%digit)
      ret%sign = "+"
    else if (val1%sign == "-" .and. val2%sign == "-") then
      ret%digit = add_(val1%digit, val2%digit)
      ret%sign = "-"
    else if (val1%sign == "+" .and. val2%sign == "-") then
      if (c == 1) then
        ret%digit = sub_(val1%digit, val2%digit)
        ret%sign = "+"
      else
        ret%digit = sub_(val2%digit, val1%digit)
        ret%sign = "-"
      end if
    else if (val1%sign == "-" .and. val2%sign == "+") then
      if (c == 1) then
        ret%digit = sub_(val1%digit, val2%digit)
        ret%sign = "-"
      else
        ret%digit = sub_(val2%digit, val1%digit)
        ret%sign = "+"
      end if
    else
      error stop "Error: add."
    end if
  end function add

  pure module function sub(val2, val1) result(ret)
    type(long_integer), intent(in) :: val2
    type(long_integer), intent(in) :: val1
    type(long_integer) :: ret
    integer(i8) :: c

    c = comp_(val2%digit, val1%digit)
    if (val1 == val2) then
      ret%digit = [0_i8]
      ret%sign = "+"
    else if (val1%sign == "+" .and. val2%sign == "+") then
      if (c == 1) then
        ret%digit = sub_(val2%digit, val1%digit)
        ret%sign = "+"
      else
        ret%digit = sub_(val1%digit, val2%digit)
        ret%sign = "-"
      end if
    else if (val1%sign == "-" .and. val2%sign == "-") then
      if (c == 1) then
        ret%digit = sub_(val2%digit, val1%digit)
        ret%sign = "-"
      else
        ret%digit = sub_(val1%digit, val2%digit)
        ret%sign = "+"
      end if
    else if (val1%sign == "+" .and. val2%sign == "-") then
      ret%digit = add_(val1%digit, val2%digit)
      ret%sign = "+"
    else if (val1%sign == "-" .and. val2%sign == "+") then
      ret%digit = add_(val1%digit, val2%digit)
      ret%sign = "-"
    else
      error stop "Error: sub."
    end if
  end function sub

end submodule long_integer_submodule

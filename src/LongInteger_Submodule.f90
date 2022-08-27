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
  pure function cut_leading_zeros_(digs) result(ret)
    integer(i8), contiguous, intent(in) :: digs(:)
    integer(i8), allocatable :: ret(:)
    integer(i32) :: i

    do i = 1, size(digs)
      if (digs(i) /= 0_i8) then
        ret = digs(i:)
        return
      end if
    end do
  end function cut_leading_zeros_

  !> The function of carry.
  pure function carry_(digs, opt) result(ret)
    integer(i8), contiguous, intent(in) :: digs(:)
    character, intent(in) :: opt
    integer(i8), allocatable :: ret(:)
    integer(i8), allocatable :: tmp(:)

    select case (opt)
    case ('+')

      ret = [[0_i8], digs]
      allocate (tmp(size(ret)))

      do
        if (all(ret < 10)) return
        tmp = 0 !> Initialization
        where (ret >= 10)
          tmp = 1
          ret = ret - 10
        end where
        tmp = cshift(tmp, 1)
        ret = ret + tmp
      end do

    case ('-')

      ret = digs
      allocate (tmp(size(ret)))

      do
        if (all(ret >= 0)) return
        tmp = 0 !> Initialization
        where (ret < 0)
          tmp = 1
          ret = ret + 10
        end where
        tmp = cshift(tmp, 1)
        ret = ret - tmp
      end do

    end select
    error stop "Error: carry_."
  end function carry_

  !> Inner function used in add_ and sub_
  pure subroutine init_(dig1, dig2, tmp1, tmp2)
    integer(i8), contiguous, intent(in) :: dig1(:), dig2(:)
    integer(i8), allocatable, intent(out) :: tmp1(:), tmp2(:)
    integer(i32) :: i, n

    associate (n1 => size(dig1), n2 => size(dig2))
      n = max(n1, n2)
      tmp1 = [[(0_i8, i=1, n - n1)], dig1]
      tmp2 = [[(0_i8, i=1, n - n2)], dig2]
    end associate
  end subroutine init_

  !> Inner function to add two positive digit arrays.
  pure function add_(dig1, dig2) result(ret)
    integer(i8), contiguous, intent(in) :: dig1(:), dig2(:)
    integer(i8), allocatable :: ret(:)
    integer(i8), allocatable :: tmp1(:), tmp2(:)

    call init_(dig1, dig2, tmp1, tmp2)
    ret = cut_leading_zeros_(carry_(tmp1 + tmp2, '+'))
  end function add_

  !> Inner function to subtract when dig2 >= dig1.
  pure function sub_(dig2, dig1) result(ret)
    integer(i8), contiguous, intent(in) :: dig2(:), dig1(:)
    integer(i8), allocatable :: ret(:)
    integer(i8), allocatable :: tmp1(:), tmp2(:)

    call init_(dig1, dig2, tmp1, tmp2)
    ret = cut_leading_zeros_(carry_(tmp2 - tmp1, '-'))
  end function sub_

  !> Inner function to compare two digit arrays.
  pure function comp_(dig1, dig2) result(ret)
    integer(i8), contiguous, intent(in) :: dig1(:), dig2(:)
    integer(i8) :: ret
    integer(i32) :: i, n1, n2

    n1 = size(dig1)
    n2 = size(dig2)

    if (n1 > n2) then
      ret = 1
      return
    else if (n1 < n2) then
      ret = -1
      return
    end if

    do i = n1, 1, -1
      if (dig1(i) > dig2(i)) then
        ret = 1
        return
      else if (dig1(i) < dig2(i)) then
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
    integer(i32) :: tmp, i
    integer(i32) :: num_digits

    val%sign = merge("+", "-", int_ >= 0)
    if (int_ == 0_i32) then
      val%digit = [0_i8]
      return
    end if

    tmp = abs(int_)
    num_digits = floor(log10(real(tmp))) + 1
    call allocate_(val, num_digits)
    do i = num_digits, 1, -1
      val%digit(i) = int(mod(tmp, 10), i8)
      tmp = tmp/10
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
    type(long_integer), intent(in) :: val1, val2

    eq = all(val1%digit == val2%digit) &
         .and. val1%sign == val2%sign
  end function eq

  !> Greater than
  pure module logical function gt(val1, val2)
    type(long_integer), intent(in) :: val1, val2
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
    type(long_integer), intent(in) :: val1, val2
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
    type(long_integer), intent(in) :: val1, val2

    ge = gt(val1, val2) .or. eq(val1, val2)
  end function ge

  !> Less than or equal to
  pure module logical function le(val1, val2)
    type(long_integer), intent(in) :: val1, val2

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
    type(long_integer), intent(in) :: val2, val1
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

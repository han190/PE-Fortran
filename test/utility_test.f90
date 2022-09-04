program main

  use utility_module
  use constant_module
  implicit none

  call title("'utility_module'")
  block
    integer, parameter :: i = 12345
    integer, allocatable :: arr(:)
    logical :: next

    arr = [1, 2, 3, 4, 5]
    call test(all(to_array(i) == arr), "to_array")
    call test(to_integer(arr) == i, "to_integer")
    call test(unit_digit(i) == 5, "unit_digit")
    call test(number_of_digits(i) == 5, "number_of_digits")

    next = next_permute(arr)
    call test(all(arr == [1, 2, 3, 5, 4]), "next_permute(idx)")

    arr = [1, 2, 3, 4, 5]
    next = next_permute(arr, 6)
    call test(all(arr == [1, 2, 3, 4, 6]), "next_permute(idx, n)")
  end block

contains

  subroutine test(input_condition, input_name)
    logical, intent(in) :: input_condition
    character(*), intent(in) :: input_name

    if (input_condition) then
      print "(a)", "Testing "//input_name//"... passed."
    else
      print "(a)", "Testing "//input_name//"... not passed!"
      stop
    end if
  end subroutine test

  subroutine title(input_name)
    character(*), intent(in) :: input_name

    print "(a)", repeat("-", 30)//new_line("a")//"Testing "//input_name
  end subroutine title

end program main

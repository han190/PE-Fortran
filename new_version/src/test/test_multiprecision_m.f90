program main

    use multiprecision_m
    implicit none

    type(multiprecision_t) :: a, b, c
    integer :: p
    logical :: cond

    call title("'euler_multiprecision_m'")
    test_addition: block
        a = "23459872394587023948572394857923457923458723455432"
        b = "23409587239485723489572934857293450234985723457632"
        c = "46869459634072747438145329715216908158444446913064"
        cond = c == a + b

        call test(cond, "addition")
    end block test_addition

    test_subtraction: block
        a = "23459872394587023948572394857923457923458723455432"
        b = "23409587239485723489572934857293450234985723457632"
        c = "-99949714844898699541000539999369992311527000002200"
        cond = c == a - b

        call test(cond, "subtraction")
    end block test_subtraction

    test_multiplication: block
        a = "23459872394587023948572394857923457923458723455432"
        b = "23409587239485723489572934857293450234985723457632"
        c = "54918592944828777958592229956265660805018656206857"// &
            "9912143134513601935683616475208660009329692257024"
        cond = c == a*b

        call test(cond, "multiplication")
    end block test_multiplication

    test_power: block
        a = "23459872394587023948572394857923457923458723455432"
        p = 12
        c = "27791228582771098796131351564405163521247196723249"// &
            "87223036707220745161360119522300106027135557338905"// &
            "65935311089076019480759260891856804808115185407046"// &
            "08649190490054254144287397101266811250646076901754"// &
            "24181773116649253700400781790669870235099026247270"// &
            "28018630205511731061179129346558847567952448848956"// &
            "60492659285963273856756812620640739475076221647126"// &
            "87045544298274187571179193061352556073368908863300"// &
            "94368159857188317292217489483960010604983574558711"// &
            "47845192831878612791307575860211356222986497999320"// &
            "71410898512698170152666726885200859146309071005450"// &
            "6574372779462222353600257430609476112613376"
        cond = c == a**p

        call test(cond, "power")
    end block test_power

contains

    subroutine test(input_condition, input_name)
        logical, intent(inout) :: input_condition
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

        print "(a)", "Testing "//input_name//"..."
    end subroutine title

end program main

module euler_prob_api_m

    use euler_interface_m
    implicit none

    type :: euler_probs_t
        procedure(euler_prob_x), nopass, pointer :: answer
    end type euler_probs_t

    abstract interface
        character(len=20) function euler_prob_x()
        end function euler_prob_x
    end interface

contains

    subroutine euler_init(probs)
        type(euler_probs_t), allocatable, intent(out) :: probs(:)

        allocate (probs(61))
        probs(1)%answer => euler0001
        probs(2)%answer => euler0002
        probs(3)%answer => euler0003
        probs(4)%answer => euler0004
        probs(5)%answer => euler0005
        probs(6)%answer => euler0006
        probs(7)%answer => euler0007
        probs(8)%answer => euler0008
        probs(9)%answer => euler0009
        probs(10)%answer => euler0010
        probs(11)%answer => euler0011
        probs(12)%answer => euler0012
        probs(13)%answer => euler0013
        probs(14)%answer => euler0014
        probs(15)%answer => euler0015
        probs(16)%answer => euler0016
        probs(17)%answer => euler0017
        probs(18)%answer => euler0018
        probs(19)%answer => euler0019
        probs(20)%answer => euler0020
        probs(21)%answer => euler0021
        probs(22)%answer => euler0022
        probs(23)%answer => euler0023
        probs(24)%answer => euler0024
        probs(25)%answer => euler0025
        probs(26)%answer => euler0026
        probs(27)%answer => euler0027
        probs(28)%answer => euler0028
        probs(29)%answer => euler0029
        probs(30)%answer => euler0030
        probs(31)%answer => euler0031
        probs(32)%answer => euler0032
        probs(33)%answer => euler0033
        probs(34)%answer => euler0034
        probs(35)%answer => euler0035
        probs(36)%answer => euler0036
        probs(37)%answer => euler0037
        probs(38)%answer => euler0038
        probs(39)%answer => euler0039
        probs(40)%answer => euler0040
        probs(41)%answer => euler0041
        probs(42)%answer => euler0042
        probs(43)%answer => euler0043
        probs(44)%answer => euler0044
        probs(45)%answer => euler0045
        probs(46)%answer => euler0046
        probs(47)%answer => euler0047
        probs(48)%answer => euler0048
        probs(49)%answer => euler0049
        probs(50)%answer => euler0050
        probs(51)%answer => euler0051
        probs(52)%answer => euler0052
        probs(53)%answer => euler0053
        probs(54)%answer => euler0054
        probs(55)%answer => euler0055
        probs(56)%answer => euler0056
        probs(57)%answer => euler0057
        probs(58)%answer => euler0058
        probs(59)%answer => euler0059
        probs(60)%answer => euler0060
        probs(61)%answer => euler0061
    end subroutine euler_init

end module euler_prob_api_m

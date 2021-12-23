module interface_m

    use constant_m
    use utility_m
    implicit none

    interface

        #: for i in range(1, NUM_PROB + 1)
        module character(len=20) function euler${"%4.4d" % (i,)}$ ()
        end function euler${"%4.4d" % (i,)}$

        #: endfor

    end interface

end module interface_m

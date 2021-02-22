program p
    integer :: s
    integer (kind = 1) :: a
    a = 1

    s = doarith(a,a)

contains
    integer function doarith(n,m)
        integer (kind = 2), intent(in) :: n
        integer (kind = 1), intent(in) :: m

        doarith = 2

    end function doarith

end program p

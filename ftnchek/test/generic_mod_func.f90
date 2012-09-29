module modu

contains
    integer function add_integer(a, b)
        integer :: a, b
        add_integer = a + b
    end function add_integer

    real function add_real(a, b)
        real :: a, b
        add_real = a + b
    end function add_real
end module modu

program prog
    use modu

    interface add
        module procedure add_integer, add_real
    end interface add

    integer :: i1, i2, ia
    real :: r1, r2, ra
    complex :: c
    i1 = 1
    i2 = 2
    r1 = 1.0
    r2 = 2.0
    c = (5.0, 3.0)

    ia = add(i1, i2)
    ra = add(r1, r2)
    c = add(c, r2)
    print *, ia, ra
end program prog

program p
    type t
        integer :: i
        integer, dimension(4) :: iarray
        character (len=10) :: string
        character (len=10), dimension(5) :: carray
    end type t

    type s
        type(t), dimension(5) :: darray
    end type s

    type(t), dimension(10) :: tvar
    type(s), dimension(6) :: svar
    type(t), dimension(5,5) :: tvar2

    tvar(1)%iarray(1:4:2) = tvar%i
    tvar%string(2:5) = "hi" 
    tvar(1)%carray(1:11:0)(1:11) = "a"
    tvar%i = tvar(1:3)%i

    tvar2(3,3)%iarray = tvar(1)%iarray

    svar(2)%darray = tvar(1:3)
    svar(2)%darray = svar(3)%darray(2:3)
    svar(2)%darray(3)%carray(1:11:2)(3:8) = "a"
    svar(2)%darray(3:5)%carray(1:11:2)(3:8) = "a"
    svar(3:6)%darray(3)%carray(1:11:2)(3:8) = "a"
    svar(3:6)%darray(3:5)%carray(11)(3:8) = "a"

end program p

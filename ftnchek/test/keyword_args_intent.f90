module modu
contains
  integer function func(i, r)
    integer, intent(out) :: i
    real, intent(in), optional :: r

    if (present(r)) then
        func = 2
    else
        func = 1
    endif
    
    i = 3

  end function
end module modu

program prog
  use modu

  integer :: a
  !correct
  a = func(a, 3.0/1.5)
  a = func(i = a, r = 2.0)
  a = func(i = a, r = 3.3 - 2.0)
  a = func(i = a, r = -3.3)

  !not correct
  a = func(5+3, r = 2.2)
  a = func(i = 1*10, r = 3.3)
  a = func(r = 4.2, i = 3)

end program prog

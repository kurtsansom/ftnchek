module modu
contains
  integer function func(i, r)
    integer, intent(in) :: i
    real, intent(in), optional :: r

    if (present(r)) then
        func = 2
    else
        func = 1
    endif
    
  end function
end module modu

program prog
  use modu

  integer :: a = 3

  !correct
  a = func(i = a, r = 3.0)
  a = func(r = 3.0, i = a)

  !not correct
  a = func(i = a, r = 2.0*2, x = 2.2)
  a = func(z = 4, 3.0)

end program prog

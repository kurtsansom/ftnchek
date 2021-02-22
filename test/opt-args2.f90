module modu
contains
  integer function func(i, r)
    integer, intent(in), optional :: i
    real, intent(in) :: r

    if (present(i)) then
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
  a = func(a, 2.2)
  a = func(r = 3.3, i = 5)
  a = func(i = a, r = 2.2)

  !incorrect
  a = func(a)
  a = func(r = 3.3, 2.2)

end program prog

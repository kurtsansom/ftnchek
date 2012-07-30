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

  integer :: a = 1

  ! all correct
  a = func(r = 2.0, i = a)
  a = func(1+1, 2.0)
  a = func(1, r = 2.0 + 3.1)
  a = func(i = 1+a, r = 2.0 - 1.0)

  ! wrong
  a = func(2.0, 1)
  a = func(i = 1.0+1.0, r = 2)
  a = func(r = 2+1, i = 1.0)
end program prog

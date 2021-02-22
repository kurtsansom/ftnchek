program prog
  integer :: i
  i = choice()
  i = choice(i)
  i = choice(i,2)
  i = choice(a2=i, b=2)
contains
  integer function choice(a1, a2, b)
    integer  :: a1, a2
    integer, optional :: b

    if (present(b)) then
        choice = b
    else
        choice = a1
        choice = a2
    end if
  end function
end program prog

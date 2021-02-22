program p
  type t
     sequence
     integer :: x
  end type t
  type(t) :: t_var
  t_var%x = myfun(1)
  write(*,*) t_var%x
contains
  function myfun(n)
    myfun = n
  end function myfun
end program p

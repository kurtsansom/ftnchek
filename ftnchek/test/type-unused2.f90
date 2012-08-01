program p
  type t
     sequence
     integer :: x
  end type t
  call foo(t(1))
end program p

subroutine foo(t_var)
  type t
     sequence
     integer :: x
  end type t
  type(t) :: t_var
  print *, t_var%x
end subroutine foo

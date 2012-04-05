program p
  type t1
    integer :: value
    type(t2), pointer :: link
  end type t1

  type t2
    integer :: value
    type(t1), pointer :: link
  end type t2

  type(t1), target :: a
  type(t2), target :: b

  a%value = 1
  a%link => b
  a%value = 2
  b%link => a

  print *, a%value, a%link%value
  print *, b%value, b%link%value
end program p

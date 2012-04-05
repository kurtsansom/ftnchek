program twotypes
  type t1
     sequence
    integer :: value
  end type t1

  type t2
     sequence
    integer :: value
    type(t1), pointer :: link
  end type t2

  type(t1),target :: a
  type(t2) :: b

  a%value = 1
  b%value = 2
  b%link => a
 
  call sub(a,b)
end program twotypes

subroutine sub(a,b)
  type t1
     sequence
    integer :: value
  end type t1

  type t2
     sequence
    integer :: value
    type(t1), pointer :: link
  end type t2


  type(t1) :: a
  type(t2) :: b

  print *, a%value
  print *, b%value, b%link%value
end subroutine sub

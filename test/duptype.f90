! Example with two types independently declared.  Both defns
! of each type agree except that only t1 has SEQUENCE attr
! so t1 types are same, t2 types are not.
program twotypes
  type t1
     sequence
     integer :: value
  end type t1
  type t2
     integer :: value
     type(t1), pointer :: link
  end type t2
  type(t1),target :: a
  type(t2) :: b
  type(t1) :: c
  a%value = 1
  b%value = 2
  b%link => a
  c = a
  call sub(c,b)

end program twotypes

subroutine sub(a,b)
  type t1
     sequence
     integer :: value
  end type t1
  type t2
     integer :: value
     type(t1), pointer :: link
  end type t2
  type(t1) :: a
  type(t2) :: b
  
  print *, a%value
  print *, b%value, b%link%value
  
end subroutine sub


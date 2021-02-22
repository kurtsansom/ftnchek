! Example with two types independently declared.  Both defns
! of each type agree except that only t1 has SEQUENCE attr
! so t1 types are same, t2 types are not.  The ordering of defns
! causes incorrect type for link in sub's definition of t2,
! since it is not known at that point that t1 will be a dup.
program twotypes
  type t1
     sequence
     integer :: value
  end type t1
  type t2t
     integer :: value
     type(t1), pointer :: link
  end type t2t
  type(t1),target :: a
  type(t2t) :: b
  type(t1) :: c
  a%value = 1
  b%value = 2
  b%link => a
  c = a
  call sub(c,b)

end program twotypes

subroutine sub(a,b)
  type t2
     integer :: value
     type(t1), pointer :: link
  end type t2
  type t1
     sequence
     integer :: value
  end type t1
  type(t1) :: a
  type(t2) :: b
  
  print *, a%value
  print *, b%value, b%link%value
  
end subroutine sub


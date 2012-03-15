program p
  type :: t
     integer :: a,b,c
  end type t
  type(t) :: tvar
  tvar%a = 1
  tvar%b = 2
  tvar%c = 3
  call sub(tvar)
contains
  subroutine sub(x)
    type(t) :: x
    print *, x
  end subroutine sub
end program p

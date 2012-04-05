program p
  type :: t
     integer :: a, b, c
  end type t
  type(t) :: s
  s%d = s%a
end program p

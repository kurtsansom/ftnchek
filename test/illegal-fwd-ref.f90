! Program with illegal forward references
program p
  type(t1) :: x                 !forward ref of type of data object
  type :: t1
     integer :: x
     type(t2) :: y              !forward ref of type of component
  end type t1
  type :: t2
     logical :: truefalse
  end type t2
end program p

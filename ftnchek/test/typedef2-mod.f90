! Declares type t2
module type_t2_module
  type :: t2
    real :: a, b
  end type t2

  type(t2) :: t2var
contains
  function t2fun(x)
    type(t2) :: t2fun, x
    t2fun = x
  end function t2fun
  function foo(y)
    integer :: foo, y
    foo = y
  end function foo
end module type_t2_module

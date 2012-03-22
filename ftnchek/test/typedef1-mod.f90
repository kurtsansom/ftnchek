! Declares type t1
module type_t1_module
  type :: t1
    real :: c1, c2
  end type t1

  type(t1) :: t1var
  real, parameter :: pi = 3
end module type_t1_module

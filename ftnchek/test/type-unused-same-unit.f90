module modu
  type t1
      integer i
  end type t1

end module modu

program prog
  use modu

  type t2
      real r
  end type t2

  type(t1) :: t1_var
  t1_var%i = 1
end program prog

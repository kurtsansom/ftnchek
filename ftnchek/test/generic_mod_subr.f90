module modu
contains
    subroutine int_switch (x, y)
      integer, intent (inout) :: x, y
      integer :: t
      t = x
      x = y
      y = t
    end subroutine int_switch
  
    subroutine real_switch (x, y)
      real, intent (inout) :: x, y
      real :: t
      t = x
      x = y
      y = t
    end subroutine real_switch
  
    subroutine complex_switch (x, y)
      complex, intent (inout) :: x, y
      complex :: t
      t = x
      x = y
      y = t
    end subroutine complex_switch
end module modu

program prog
  use modu
  interface switch
      module procedure int_switch, real_switch, complex_switch
  end interface switch

  integer :: i1, i2
  real :: r1, r2
  character :: c

  call switch(i1, c)
  call switch(i1, i2)
  call switch(r1, r2)
end program prog

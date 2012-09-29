program prog
  interface switch
    subroutine int_switch (x, y)
      integer, intent (inout) :: x, y
    end subroutine int_switch
  
    subroutine real_switch (x, y)
      real, intent (inout) :: x, y
    end subroutine real_switch
  end interface switch

  integer :: i1, i2
  real :: r1, r2
  character :: c

  call switch(i1, i2)
  call switch(r1, r2)
  call switch(c, r2)
end program prog

subroutine int_switch (x, y)
  integer, intent (inout) :: x, y
  integer t
  t = x
  x = y
  y = t
end subroutine int_switch

subroutine real_switch (x, y)
  real, intent (inout) :: x, y
  real t
  t = x
  x = y
  y = t
end subroutine real_switch

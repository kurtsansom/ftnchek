module defns
  type point
     real :: x, y
  end type point
  type triangle
     type(point) :: a, b, c
  end type triangle
contains
  type(triangle) function tfunc(p)
    type(point) :: p
    tfunc%a = p
    tfunc%b = p
    tfunc%c = p
  end function tfunc
  real function rfunc(p)
    real :: p
    rfunc = p
  end function rfunc
end module defns

program geom
  use defns, pt=>point, tr=>triangle
  type point
     character(len=10) :: s
  end type point
  type triangle
     real :: x
  end type triangle
  type(pt) :: p1
  type(tr) :: t1
  type(triangle) :: bermuda
  type(point) :: sharp
  t1 = tfunc(p1)
  t1%a = p1
  sharp%s = "foo"
  bermuda%x = 1.23
  bermuda%x = rfunc(p1)  ! arg mismatch: actual pt/point, dummy real
  t1 = tfunc(sharp)      ! arg mismatch; actual local point, dummy module point
  t1%a = sharp           ! assign mismatch: local point to module point
end program geom

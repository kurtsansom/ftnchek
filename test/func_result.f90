program ftest
  real :: x, y
  real :: sqr
  external :: sqr
  x = 2.0
  y = sqr(x)
  print *, x, y
end program ftest

function sqr(x) result(xsqr)
  real, intent(in) :: x
  real :: xsqr
  xsqr = x*x
end function sqr

program optkind

  real x, y
  integer :: i, j = 10
  real :: r, s = 1.5
  complex :: c = ( 1.1, 2 )

  x = real(4)
  x = real(y,4)
  x = real(kind=4,y)            !need keyword on 2nd arg
  x = real(8,kind=4)
  r = real(a = s, kind = 8)

  ! correct
  s = aint(s, kind = 4)
  s = anint(kind = 8, a = s)
  i = int(kind = 8, a = j)
  i = ceiling(kind = 4, a = s)
! CMPLX tests
  c = cmplx(x,y)                ! OK
  c = cmplx(x,y,4)              ! mixed concrete, default
  c = cmplx(c, kind = 4)        ! ditto
  c = cmplx(kind = 4, x = c)    ! ditto
  c = cmplx(x=1,y=0)            ! OK

  i = floor(r, kind = 4)
  i = ceiling(kind=4, a=2.5_2)

  ! incorrect keywords
  i = floor(r, kin = 4)
  i = int(a = j, kin = 4)

end program optkind

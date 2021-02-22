module elemental_mod
contains
  elemental subroutine s(a,b,c)
    integer, intent(in) :: a, b
    integer, intent(out) :: c
    c = a + b
  end subroutine s
  elemental function f(a,b) result(f_res)
    integer, intent(in) :: a, b
    integer :: f_res
    f_res = a - b
  end function f
end module elemental_mod

program p
  use elemental_mod
  integer, dimension(3) :: x, y, z
  integer, dimension(2) :: w
  integer :: n
  read *, x, y
  call s(x,y,z)                 !elemental subroutine: OK
  print *, x, '+', y, '=', z
  call s(x,2,z)                 !elemental subroutine: OK
  print *, x, '+ 2 2 2 =', z
  z = f(x,y)                    !elemental function: OK
  print *, x, '-', y, '=', z
! Up to here OK, runs and prints vector sum & diff
! Now for some errors

  call s(x,y,w)      !elemental subroutine: arg size mismatch
  call s(x,y,n)      !elemental subroutine: out arg size mismatch
  z = f(x,w)         !elemental function: arg size mismatch
  w = f(x,y)         !elemental function: size mismatch on assignment
end program p

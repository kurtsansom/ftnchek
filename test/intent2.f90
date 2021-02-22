subroutine s(x,y,z)
  intent(in) :: x
  intent(inout) :: y
  intent(out) :: z
  intent(out) :: x,y,z
  z = x + y
end subroutine s

subroutine s(x,y,z)
  real, intent(in) :: x
  real, intent(inout) :: y
  real, intent(out) :: z
  integer, intent(out) :: non_arg
  ! errors: assigning IN arg x, using OUT arg z before set
  x = y + z
end subroutine s

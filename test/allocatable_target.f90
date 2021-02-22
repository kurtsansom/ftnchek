program prog
  integer, target, allocatable, dimension(:) :: a
  integer, pointer, dimension(:) :: p
  allocate( a(10) )
  p => a
  p = 1
  print *, p
end program prog

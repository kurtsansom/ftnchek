program allocstring
  character, dimension(:), allocatable :: s
  allocate(s(1:10))
  s = 'x'
  print *, s
end program allocstring

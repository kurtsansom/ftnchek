program boz_constants
  integer a(4), b(4)
  data a / B'101010', O'123456', Z'123abc', X'123abc' /
  data b / '101010'B, '123456'O, '123abc'Z, '123abc'X /
  print *, a
  print *, b
end program boz_constants

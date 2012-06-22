! Testing lookahead parsing of char-selector for different
! possible cases of len and kind specs.
program charfuns
  implicit none
  character*10 c
  c = f1("hello")
  c = f2(c)
  c = f3(c)
  c = f4(c)
  c = f5(c)
  c = f6(c)
  c = f7(c)
  c = f8(c)                     !not found by lookahead
  print *, c
contains
  character (len=10) function f1(c)
    character (len=*) c
    f1 = c
  end function f1
  character (len=10, kind=1) function f2(c)
    character (len=10) c
    f2 = c
  end function f2
  character (10, kind=1) function f3(c)
    character (len=10) c
    f3 = c
  end function f3
  character (10, 1) function f4(c)
    character (len=10) c
    f4 = c
  end function f4
  character (kind=1) function f5(c)
    character (len=10) c
    f5 = c
  end function f5
  character (kind=1, len=10) function f6(c)
    character (len=10) c
    f6 = c
  end function f6
  character (len=10, 1) function f7(c) !illegal to omit KIND= here
    character (len=10) c
    f7 = c
  end function f7
  character (kind=1, 10) function f8(c) !illegal to omit LEN= here
    character (len=10) c
    f8 = c
  end function f8
end program charfuns

! external function
character*10 function f8(c)
    character (len=10) c
    f8 = c
end function f8

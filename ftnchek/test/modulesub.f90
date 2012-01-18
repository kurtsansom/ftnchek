module mod
contains
  subroutine modsub1
    print *, "Modsub1 called"
    call intsub1
    call modsub2
  contains
    subroutine intsub1
      print *, "Intsub1 called"
      call intsub2
      call modsub2
    end subroutine intsub1
    subroutine intsub2
      print *, "Intsub2 called"
    end subroutine intsub2
  end subroutine modsub1
  subroutine modsub2
    print *, "Modsub2 called"
  end subroutine modsub2
end module mod

program mainprog
  use mod
  call modsub1
end program mainprog

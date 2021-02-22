program main
    call sub1
    call extsub1
contains
    subroutine sub1
        print *, "Sub1 called"
        call sub2
    end subroutine sub1
    subroutine sub2
        print *, "Sub2 called"
        call extsub2
    end subroutine sub2
end program main

subroutine extsub1
    print *, "extsub1 called"
end subroutine extsub1
subroutine extsub2
    print *, "extsub2 called"
end subroutine extsub2

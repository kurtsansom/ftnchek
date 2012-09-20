program prog
  interface 

      integer function f(i)
        real :: i
      end function f

      subroutine s(j)
        integer :: j
      end subroutine s

  end interface

  integer :: a

  a = f(3.0)
  print *, a

  call s(2)

end program prog

integer function f(i)
  real :: i
  f = int(i)
end function

subroutine s(j)
  integer :: j
end subroutine s

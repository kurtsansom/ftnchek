! Program to exercise lookahead parsing of subprogram defns
! to get type, kind, and size
program internalfuns
  implicit none
  integer :: n
  real*8 r8
  real(kind=4) k4
  type f_type
     integer :: x
  end type f_type
  type(f_type) :: f_var
  character(len=10,kind=1) :: c
  n = f(3)
  n = g(3)                      ! implicit declaration of external
  r8 = fr8(3.0d0)
  k4 = fk4(3.0_4) + 0.0
  k4 = k4 + 0.0
  f_var = ft(4)
  c = c_fun("hello")
  c = c_k1("hello") // "hello"
  c = c_k1_l10("hello")
  print *, n, r8, k4, f_var%x
  print *, c
contains
  integer function f(i)
    integer :: i
    f = i*i
  end function f

  real*8 function fr8(i)
    implicit none
    real*8 i
    fr8 = i*i
  end function fr8

  real(kind = 4 ) function fk4(i)
    real(kind=4) i
    fk4 = i*i
  end function fk4

  type(f_type) function ft(i)
    implicit none
    integer :: i
    ft%x = i*i
  end function ft

  character*10 function c_fun(str)
    character (len=*) :: str
    c_fun = str(1:5) // str(1:5)
  end function c_fun

  character ( kind = 1 ) function c_k1(str)
    character (len=*) :: str
    c_k1 = str(1:5) // str(1:5)
  end function c_k1

  character ( kind = 1, len = 10 ) function c_k1_l10(str)
    character (len=*) :: str
    c_k1_l10 = str(1:5) // str(1:5)
  end function c_k1_l10

end program internalfuns

! external function should not be found by lookahead
integer function g(n)
  g = n*3
end function g

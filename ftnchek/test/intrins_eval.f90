! Program to test evaluation of integer intrinsics for parameter expressions.
! It also tests propagation of constant expression values.
! Various named constants are defined using the intrinsics and then used to
! declare lengths of character variables, so values appear in symbol table.
! Evaluated integer operators are:
!    + - * / **, char operator: // (for len)
! Evaluated intrinsics are:
!    abs dim ichar index len max min mod sign

program intrins_eval
  integer, parameter :: five = len('12345'), ten = 2*five
  integer, parameter :: fifteen = max(five,ten,15), two = min(five,ten,5/2,11)
  integer, parameter :: seven = abs(-7), thirteen = dim(30,36/2-1)+dim(10,11)
  integer, parameter :: twenty_five = ichar('Z') - ichar('A')
  integer, parameter :: three = index('abcdef','c'), four = mod(five**2,7)
  integer, parameter :: six = mod(41,seven), twenty_one = sign(23,1)+sign(2,-1)
  character :: c5*(five), c10*(ten), c15*(fifteen), c6*(six)
  character :: c7*(seven), c13*(thirteen), c25*(twenty_five)
  character :: c3*(three), c4*(four), c2*(two), c21*(twenty_one)
  complex, parameter :: imag_unit = (0.0, 1.0)
  double complex :: dc = (3.14d0,1.414d0)
  real :: r
  double precision :: d
! Test that return value of CHAR(n) is size 1.
  c2 = char(46) // char(47) // char(48) ! truncation warning
! Test that REAL(intg) returns real
  d = real(20)                  ! gives promotion warning
! Test that ABS(real), REAL(real) return real, IMAG(real) is error
  r = abs(20.0)                 ! OK
  r = imag(20.0)                ! type mismatch
  r = real(20.0)                ! OK
  d = abs(20.0)                 ! promotion warning
  d = real(20.0)                ! promotion warning
! Test that ABS(cplx), IMAG(cplx), REAL(cplx) return real
  r = abs(imag_unit)            ! OK
  r = imag(imag_unit)           ! OK
  r = real(imag_unit)           ! OK
  d = abs(imag_unit)            ! promotion warning
  d = imag(imag_unit)           ! promotion warning
  d = real(imag_unit)           ! promotion warning
! Test that ABS(dcpx), IMAG(dcpx), REAL(dcpx) return real
  r = abs(dc)                   ! truncation warning
  r = imag(dc)                  ! truncation warning
  r = real(dc)                  ! truncation warning
  d = abs(dc)                   ! OK
  d = imag(dc)                  ! OK
  d = real(dc)                  ! OK
end program intrins_eval

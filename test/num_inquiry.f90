program num_inquiry
! DIGITS (X)                         Number of significant digits of the model  
! EPSILON (X)                        Number that is almost negligible        
!                                       compared to one                              
! HUGE (X)                           Largest number of the model             
! MAXEXPONENT (X)                    Maximum exponent of the model                   
! MINEXPONENT (X)                    Minimum exponent of the model              
! PRECISION (X)                      Decimal precision                 
! RADIX (X)                          Base of the model                 
! RANGE (X)                          Decimal exponent range                    
! TINY (X)                           Smallest positive number of the model
  implicit none
  integer :: I
  real :: X
  double precision :: D
  complex :: C
  print *, "X = real"
  print *, "DIGITS (X)=", DIGITS (X)
  print *, "EPSILON (X) =", EPSILON (X) 
  print *, "HUGE (X) =", HUGE (X) 
  print *, "MAXEXPONENT (X) =", MAXEXPONENT (X) 
  print *, "MINEXPONENT (X) =", MINEXPONENT (X) 
  print *, "PRECISION (X) =", PRECISION (X) 
  print *, "RADIX (X) =", RADIX (X) 
  print *, "RANGE (X) =", RANGE (X) 
  print *, "TINY (X) =", TINY (X) 

  print *, "D = double precision"
  print *, "DIGITS (D)=", DIGITS (D)
  print *, "EPSILON (D) =", EPSILON (D) 
  print *, "HUGE (D) =", HUGE (D) 
  print *, "MAXEXPONENT (D) =", MAXEXPONENT (D) 
  print *, "MINEXPONENT (D) =", MINEXPONENT (D) 
  print *, "PRECISION (D) =", PRECISION (D) 
  print *, "RADIX (D) =", RADIX (D) 
  print *, "RANGE (D) =", RANGE (D) 
  print *, "TINY (D) =", TINY (D) 

  print *, "I = integer"
  print *, "DIGITS (I)=", DIGITS (I)
  print *, "EPSILON (I) =", EPSILON (I)  ! no-no: must be real
  print *, "HUGE (I) =", HUGE (I) 
  print *, "MAXEXPONENT (I) =", MAXEXPONENT (I)  ! no-no: must be real
  print *, "MINEXPONENT (I) =", MINEXPONENT (I)  ! no-no: must be real
  print *, "PRECISION (I) =", PRECISION (I)  ! no-no: must be real or complex
  print *, "RADIX (I) =", RADIX (I)
  print *, "RANGE (I) =", RANGE (I) 
  print *, "TINY (I) =", TINY (I)  ! no-no: must be real

  print *, "C = complex"
  print *, "DIGITS (C)=", DIGITS (C)  ! no-no: must be real
  print *, "EPSILON (C) =", EPSILON (C)   ! no-no: must be real
  print *, "HUGE (C) =", HUGE (C)   ! no-no: must be real
  print *, "MAXEXPONENT (C) =", MAXEXPONENT (C)   ! no-no: must be real
  print *, "MINEXPONENT (C) =", MINEXPONENT (C)   ! no-no: must be real
  print *, "PRECISION (C) =", PRECISION (C) 
  print *, "RADIX (C) =", RADIX (C)   ! no-no: must be real
  print *, "RANGE (C) =", RANGE (C)
  print *, "TINY (C) =", TINY (C)   ! no-no: must be real
end program num_inquiry

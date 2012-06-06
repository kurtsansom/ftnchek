program elemental_intrinsics
  real a(10), b(10), c(20)
  b = 1; c = 1                ! suppress used-before-set warnings

  ! verify array propagation and checking in unary and binary
  ! expressions and assignments.
  a = b                         !unary expr: OK
  a = c                         !size mismatch in assignment
  a = -b                        !unary expr: OK
  a = -c                        !unary expr: size mismatch in assignment
  a = a + b                     !binary expr: OK
  a = b + c                     !binary expr: size mismatch in expr

  ! verify array propagation and checking in elemental intrinsic
  ! functions.

  a = abs(b)                    !1-arg intrinsic: OK
  a = abs(c)                    !1-arg intrinsic: size mismatch in assignment
  a = max(a,b)                  !2-arg intrinsic: OK
  a = max(b,c)                  !2-arg intrinsic: size mismatch in args
  c = min(a,b)                  !2-arg intrinsic: size mismatch in assignment
  b = min(a,b,c)                !3-arg intrinsic: size mismatch in args

  b = a                         ! suppress unused warning
end program elemental_intrinsics

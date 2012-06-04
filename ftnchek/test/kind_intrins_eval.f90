! This program makes no sense but serves to verify that the kind
! functions are calculating their values.  The character declarations
! will be illegal negative values on any real compiler.  Use -symt
! to see the values produced, displayed as character var sizes.
program kind_intrins_eval
  real :: x
! KIND(X)
  integer, parameter :: &
       k_defi = -kind(1), &
       k_defr1 = -kind(1.0), &
       k_defrx = -kind(x), &
       k_defrd = -kind(1.0d0), &
       k_defl = -kind(.true.), &
       k_defc = -kind("hello")
  character :: c_defi*(k_defi), c_defr1*(k_defr1), c_defrx*(k_defrx), &
       c_defrd*(k_defrd), c_defl*(k_defl), c_defc*(k_defc)
! SELECTED_INT_KIND(R)
  integer, parameter :: &
       k_i5 = -selected_int_kind(5), &
       k_i10 = -selected_int_kind(10), &
       k_i15 = -selected_int_kind(15), &
       k_i0 = -selected_int_kind(0), &
       k_i98765 = -selected_int_kind(98765)
  character :: c_i5*(k_i5), c_i10*(k_i10), c_i15*(k_i15), &
       c_i0*(k_i0), c_i98765*(k_i98765)
! SELECTED_REAL_KIND(P)
  integer, parameter :: &
       k_p6 = -selected_real_kind(6), &
       k_p10 = -selected_real_kind(10), &
       k_p15 = -selected_real_kind(15), &
       k_p0 = -selected_real_kind(0), &
       k_pneg = -selected_real_kind(-3), &
       k_p98765 = -selected_real_kind(98765)
  character :: c_p6*(k_p6), c_p10*(k_p10), c_p15*(k_p15), c_p0*(k_p0), &
       c_pneg*(k_pneg), c_p98765*(k_p98765)
! SELECTED_REAL_KIND(P,R)
  integer, parameter :: &
       k_p6_r37 = -selected_real_kind(6,37), &
       k_p10_r15 = -selected_real_kind(10,15), &
       k_p15_r10 = -selected_real_kind(15,10), &
       k_p0_r0 = -selected_real_kind(0,0), &
       k_p98765_r54321 = -selected_real_kind(98765,54321)
  character :: c_p6_r37*(k_p6_r37), c_p10_r15*(k_p10_r15), &
       c_p15_r10*(k_p15_r10), c_p0_r0*(k_p0_r0), &
       c_p98765_r54321*(k_p98765_r54321)
end program kind_intrins_eval

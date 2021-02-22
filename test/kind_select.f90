integer, parameter :: long = selected_int_kind(10)
integer, parameter :: dp = selected_real_kind(10)
integer, parameter :: mykind = 4

integer (kind=dp) :: lvar     ! warn : integer using kind for real
real (kind=long) :: dpvar     ! warn : real using kind for integer
integer (kind=long) :: lid

integer (kind=3) :: cvar      ! warn : portability
real (kind=mykind) :: c2var   ! warn : portability

cvar = lid + cvar             ! warn : mixing selected and concrete

end

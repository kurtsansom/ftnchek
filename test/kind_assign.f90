integer, parameter :: ikl = selected_int_kind(8)
integer, parameter :: ikh = selected_int_kind(18)
integer, parameter :: rkl = selected_real_kind(20)
integer, parameter :: rkh = selected_real_kind(50)

integer (kind = ikl) :: il1, il2
integer (kind = ikh) :: ih1, ih2
real (kind = rkl) :: rl1, rl2
real (kind = rkh) :: rh1, rh2
real (kind(1.0q0)) :: q1

integer :: id1, id2
real :: rd1, rd2

! kind conversion to different range
ih1 = il1
il1 = ih1
il1 = il1 + ih1
il1 = id1 + il1

! kind conversion to different precision
rd1 = rl1
rl1 = rh1
rh1 = rl1

rl1 = 1.0q5
q1 = 1.0q5
q1 = rl1

end

program p
  type t
     integer, dimension(3,7) :: a
     integer :: i
  end type t
  type(t), dimension(2) :: q
  integer, dimension(3,7) :: a
  q(1)%a(2,3) = 2
  !q(1)%a = reshape((/ (i, i=1,21) /),(/ 3, 7 /) )
  q%a(1,2) = (/ 5, 6 /)
  q%a = 10 !illegal
  a = a + q(1)%a
  a = 1 + q(1)%i
end program p

program prog
  type t
    integer, pointer:: p
    integer, allocatable, dimension(:) :: a
  end type t

  integer, allocatable, dimension(:) :: b

  type(t) :: tvar

  allocate(tvar%a(5), b(10))

  do i = 1, 5
      tvar%a(i) = i
      print *, tvar%a(i)
  end do

  deallocate(tvar%a)
end program prog

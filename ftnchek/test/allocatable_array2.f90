! Example of using an array before allocation

program pointer_alloc
  real, allocatable, dimension(:, :) :: A
  integer :: M, N
  integer :: alloc_err, dealloc_err

  read *, M, N
  allocate ( A(M, N) , stat=alloc_err) 
  if( alloc_err /= 0 ) then
     print *, "Error allocating real array of size", &
          M, "x", N
  else
     read *, A
     print *, A
     deallocate (A, stat = dealloc_err)
     if( dealloc_err /= 0 ) then
        print *, "Error deallocating array"
     else
        print *, "Array deallocated"
     end if
  end if

  if( allocated(A) ) then
     print *, "Allocatable array is still allocated"
     deallocate(A)
  end if
end program pointer_alloc

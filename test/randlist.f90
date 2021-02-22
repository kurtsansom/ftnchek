program randomize_list
  implicit none
  integer, dimension(1000) :: numlist
  real, dimension(:), allocatable :: rand_num_list
  integer, dimension(:), allocatable :: rand_index_list
  integer :: i, r, n
  integer :: temp
  integer :: read_status

  print *, "Please enter some numbers, one per line:"

  do i=1, size(numlist)
     read(unit=*,fmt=*,iostat=read_status) numlist(i)
     if( read_status < 0 ) exit ! EOF
     n = i
  end do

  if( n > 0 ) then
     allocate( rand_num_list(n) )
     allocate( rand_index_list(n) )
     call random_seed
     call random_number(rand_num_list)
     rand_index_list = int(rand_num_list*n) + 1
     if( any(rand_index_list < 1 .or. rand_index_list > n) ) then
        print *, "Error in converting random numbers to random ints"
        do i=1, n
           write(unit=*,fmt=*) i, rand_index_list(i)
        end do
        stop
     end if
     do i=1,n
        r = rand_index_list(i)
        temp = numlist(i)
        numlist(i) = numlist(r)
        numlist(r) = temp
     end do
     do i=1,n
        print *, numlist(i)
     end do
  end if
end program randomize_list

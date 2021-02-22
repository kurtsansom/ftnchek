      program intern
      character*1000 buf
      namelist /names/ a
      integer a(50)
      do 10 i=1,50
         a(i) = i
 10   continue
      write(unit=buf) a
      write(unit=buf,fmt=*)
      write(buf,names)
      write(unit=*,fmt=*,nml=names)
      print *, buf
      end

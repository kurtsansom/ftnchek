      PROGRAM WRFILE
      CHARACTER FNAME*80
      WRITE(*,*) 'Enter Filename'
      READ(*,*) FNAME
C Use of NAME in OPEN is VMS Fortran extension
      OPEN (UNIT=8,NAME=FNAME,ERR=999)
      WRITE(8,100) FNAME
 100  FORMAT(1x,'Sample Output to File ',A80)
      STOP
 999  WRITE(*,*) 'Error opening file'
      STOP 2
      END

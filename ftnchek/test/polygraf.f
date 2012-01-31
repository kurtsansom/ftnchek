C polygraf.for:
C
C  Graphs the polynomial  (x-x0)(x-x1)...(x-xn) where xi are equally-spaced.
C
C  link with FUNGRAF,COREGFX
C
C  This is the well-known "error polynomial:" the error in interpolation
C  of a function by a polynomial of order n is bounded by a limit
C  proportional to this polynomial.  The lessons to learn are: 1) see
C  how it takes off outside [-1,1] illustrating the hazard of using
C  polynomials for extrapolation, and 2) observe that within [-1,1] it
C  has smallest amplitude near x=0, in contrast to the Chebyshev polynomials
C  which are well-balanced over the whole interval [-1,1].
C
C  NOTE: If your compiler does not accept identifiers over 6 chars
C  long, all such identifiers herein can be safely truncated to 6
C  characters by chopping off their ends.
C
C This program is associated with the CORE graphics package.  See
C the CORE documentation for explanation of the graphics routines.
C
C                         CORE Version 1.1
C                          January, 1990
C 
C Robert Moniot                 NOTE: This is free software.  It may
C Fordham University            be freely copied so long as it is not
C College at Lincoln Center     being directly sold for profit.  No
C New York, NY 10023            guarantees accompany the software.
C                               If you find bugs, please notify me,
C MONIOT@FORDMULC.BITNET        and I will try to fix them when I get
C                               the chance.  This does not apply to
C                               problems which may arise in porting
C                               the code to non-VMS systems.
      parameter (MAX=20)
      external poly,cheby
      real poly,cheby, x(0:MAX)
      common /polyn/ n
      character *4 namdev
      character choice
C
C  pick up the standard logical unit numbers.
C
      call inqgfu(igrafu)
      call inqeru(mesgu)
      call inqinu(inptu)
C
C       Give instructions to user.
C
      write(mesgu, *)
     $      'This program will graph either the error polynomial'
      write(mesgu, *) 'or the Chebyshev polynomial.  Which do you want?'
      write(mesgu, *) 'Enter E for error poly and C for Chebyshev'
      read(inptu,1000) choice
1000  format(a1)
      if(choice .eq. 'e') choice = 'E'
      write(mesgu,*)
      if(choice .eq. 'E') then
      write(mesgu,*)
     $ 'Graph of the polynomial psi(x)=x(x-h)(x-2h)...(x-nh)'
      write(mesgu,*)
     $ 'where nh=1.  The error in interpolation by a polynomial'
      write(mesgu,*)
     $ 'of order n is bounded by a term proportional to psi(x).'
      else
      write(mesgu,*)
     $'Graph of the Chebyshev polynomial.  The family of Chebyshev'
      write(mesgu,*)
     $'polynomials is well suited to approximation of functions. The'
      write(mesgu,*)
     $'zeroes of a Chebyshev polynomial are not equally spaced, but'
      write(mesgu,*)
     $'chosen so that the polynomial oscillates between +1 and -1 on'
      write(mesgu,*)
     $'the interval [-1,1].'
 
      endif
 
      call grafinstrs
C
C       Get device from user: graph can be made on various types.
C
      call grafdev(namdev)
      call getspd(igrafu,ibaud)
C
 
1     continue
      write(mesgu, *) 'Enter order n (-1 to stop)'
      read(inptu,*) n
      if(n .lt. 0) stop
      if(n .gt. MAX) then
        write(mesgu, *) 'Enter n between 0 and',MAX
        goto 1
      endif
 
      if(n .ne. 0) then
        h = 2.0/n
      else
        h = 1.0
      endif
 
      do 10 i=0,n
        x(i) = -1.0 + i*h
10    continue
 
      xmax = 1.0+h
      xmin = -1.0-h
      if(choice .eq. 'E') then
          ymax = abs(poly(1.0 - 0.5*h,x))*1.1
      else
          ymax = abs(cheby(1.0 + 0.1*h,x))*1.1
      endif
      if(n .eq. 0) ymax = ymax*1.5
      ymin = -ymax
C
C       Draw the graph
C
      call startgraf(xmin,xmax,ymin,ymax,namdev,ibaud)
 
      if(choice .eq. 'E') then
          call fungraf(poly,x,1)
          call legend('Error Polynomial')
      else
          call fungraf(cheby,x,1)
          call legend('Chebyshev Polynomial')
      endif
 
      call endgraf
      goto 1
      end
C
C       Function to be graphed is defined here.
C
      function poly(x,p)
      common /polyn/ n
      real p(1)
C
      poly = 1.0
      do 10 i=0,n
        poly = poly * (x - p(i+1))
10    continue
      return
      end
C
C  Chebyshev polynomial computed here.  Uses the recurrence formula:
C
C      T  (x) = 2*x*T (x) - T  (x)
C     n+1            n       n-1
C
      function cheby(x,p)
      common /polyn/ n
      real p(1)
C
 
      if(n .eq. 0) then
          cheby = 1.0
          return
      else
          t1 = 1.0
          cheby = x
          do 10 i=2,n
        t0 = t1
        t1 = cheby
        cheby = 2.0*x*t1 - t0
10        continue
      endif
      return
      end
 
 
 

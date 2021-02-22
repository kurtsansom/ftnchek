C  fungraf.for:
C
C     Library package of routines for graphing a function of your choosing.
C
C     This package is built on top of the CORE package of primitives.
C
C     Link with CORE library and object module containing
C       your main program and function(s) to be graphed.  See
C       GRAFTEST.FOR for a sample main program and function.
C
C  NOTE: this version is for VMS systems.  For other systems, modify
C  the instructions to user appropriately, and alter formats using the
C  $ operator.  (The $ is OK for Unix systems.)  If your compiler does
C  not accept identifiers over 6 chars long, all such identifiers herein
C  can be safely truncated to 6 characters by chopping off their ends.
C
C  Contents:
C     grafinstrs        -- prints instructions for the user
C     grafdev           -- gets the device type from user
C     startgraf -- initializes the graph
C     fungraf           -- performs the graphing
C     legend            -- puts a title and the axis ranges on the graph
C     endgraf           -- finishes the graph
C
C  Calling sequences:
C
C
C     call grafinstrs
C
C  Gives instructions to the user for setting terminal and about the behavior
C  of subroutine fungraf.  Call this once per run of program, at the beginning.
C
C
C     call grafdev(namdev)
C
C  Queries user about the device type for plotting, and sets namdev
C  accordingly.  namdev is a character*4 return-variable to be passed
C  on to startgraf.  Call grafdev prior to calling startgraf, unless your
C  program is device-specific, in which case you can set namdev by an
C  assignment statement to one of the values listed below under startgraf.
C
C
C     call startgraf(xmin,xmax,ymin,ymax,namdev,ibaud)
C
C  Initializes a new graph.
C
C     xmin,xmax = range of x values to be plotted.
C     ymin,ymax = range of y values to be plotted.
C     namdev = terminal name.  Set to one of  'VT52' 'VT100' 'GIGI' '4010',
C              'DUMB', or use value returned by call to grafdev which gets
C              it from user.  Namdev must be of type character*4.
C     ibaud = terminal baud rate.  Set it by an assignment statement or
C       use the value returned by getspd.
C
C     call fungraf(f,a,nstyle)
C
C  This routine plots function f(x,a) on the graph using line style nstyle.
C
C     f = external function to be plotted.
C     a = array of parameters for f.  f is invoked as y = f(x,a)
C         where x is an abscissa value.  Abscissas are generated
C         automatically in the range from xmin to xmax as set by
C         the call to startgraf.  The array of parameters is not
C         accessed by fungraf, so a can be a dummy if f does not
C         use it.
C     nstyle = integer line-style number (1 to 9).  N.B. axes are
C       drawn by startgraf with style 2.
C
C  Note that successive calls to fungraf without intervening calls to
C  endgraf and startgraf will cause each graph to be overlaid on the
C  others.
C
C     call legend(title)
C
C   Puts a legend on the graph.  Title is any character string to be
C   used as a title for the graph.  The legend also gives the X and Y
C   axis ranges.  Preferably call legend just before endgraf, so that
C   the graph will not potentially obscure the text.
C
C     call endgraf
C
C   This routine causes the graph to be displayed.  Then it waits
C   for the return key to be pressed, and restores the terminal
C   to text mode.
C
 
C This package is associated with the CORE graphics package.  See
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
C 
 
      subroutine grafinstrs
C
C  Gives instructions to the user for setting terminal and the behavior
C  of subroutine fungraf
C
         call inqeru(mesgu)
C
C  The following message is for VMS and should be removed for Unix versions.
C
         write(mesgu,902)
902      format(//
     1' For best results, instruct the system beforehand with the',
     2' command'/' $ SET TERMINAL/FORMFEED/NOWRAP'/
     3' Also, for Visual 550',
     4' terminals, the SETUP F6 key should be set to AUTO.'//)
C
C
      write(mesgu,903)
903   format(' When graph is done, hit return to get back to system.'//)
      return
      end
 
      subroutine grafdev(namdev)
C
C  Queries user about the device type for plotting, and sets namdev
C  accordingly.
C
C
C  allow various terminals
C
      character *1 iterm
      character *4 namdev
C
         call inqeru(mesgu)
         call inqinu(inptu)
    5    continue
        write(mesgu,9029)
9029  FORMAT(/' Specify terminal type: 1=VT52, 2=VT100, 3=GIGI,',
     1' 4=Tektronix 4010, 5=Dumb: ',$)
        read(inptu,901) iterm
901     format(a1)
        if(iterm .eq. '1') then
                namdev = 'VT52'
        else if(iterm .eq. '2') then
                namdev = 'V100'
        else if(iterm .eq. '3') then
                namdev = 'GIGI'
        else if(iterm .eq. '4') then
                namdev = '4010'
        else if(iterm .eq. '5') then
                namdev = 'DUMB'
        else
                write(mesgu,9022)
9022    FORMAT(' Please enter a number between 1 and 5')
                iterm = ' '
        endif
         if(iterm .eq. ' ') goto 5
C
      return
      end
 
      subroutine startgraf(xmin,xmax,ymin,ymax,namdev,ibaud)
C
C  This routine is used to initialize a new graph.
C
C     xmin,xmax = range of x values to be plotted.
C     ymin,ymax = range of y values to be plotted.
C     namdev = terminal name.  Set to one of  'VT52' 'VT100' 'GIGI' '4010'.
C               Or use subroutine grafdev to get it from user.
C     ibaud = terminal baud rate used by GFWAIT.  You can get it with GETSPD.
C     NOTE: call subroutine grafinstrs first to give instructions to user,
C     and call subroutine grafdev to get namdev from the user.
C
      character *4 namdev
      save /fngrfx/
      common /fngrfx/ xlo,xhi,ylo,yhi,npts
C
C  initialize
C
      call init(namdev,ibaud)
      call setwin(xmin,xmax,ymin,ymax)
      call newvw2
C
C  clear the screen
      call nwfrm
 
C  draw axes
      if(xmin .le. 0.0 .and. xmax .ge. 0.0) then
        call ststyl(2)
        call move2a(0.0,ymin)
        call line2a(0.0,ymax)
      endif
      if(ymin .le. 0.0 .and. ymax .ge. 0.0) then
        call ststyl(2)
        call move2a(xmin,0.0)
        call line2a(xmax,0.0)
      endif
C
C  Calculate stepsize for plot based on device resolution
C
      if(namdev .eq. 'VT52' .or. namdev .eq. 'V100') then
C               Low-res devices: use only 80 abscissae
          npts = 80
      else
C               Hi-res devices: use 500 abscissae
          npts = 500
      endif
C
C       Set up the common block values for use by fungraf
C
      xlo = xmin
      xhi = xmax
      ylo = ymin
      yhi = ymax
      return
      end
 
 
      subroutine fungraf(f,a,nstyle)
C
C  This routine plots f(x,a) on the graph using line style nstyle.
C
C     f = external function to be plotted.
C     a = array of parameters for f.  f is invoked as y = f(x,a)
C     nstyle = integer line-style number (1 to 9).  N.B. axes are
C       drawn by startgraf with style 2.
C
      external f
      real f, a(1)
      save /fngrfx/
      common /fngrfx/ xlo,xhi,ylo,yhi,npts
C
C  Now draw the function
C
      call ststyl(nstyle)
      call move2a(xlo,f(xlo,a))
      xstep = (xhi-xlo)/npts
      Do 20 i=0,npts
          x = xlo + xstep*i
          call line2a(x,f(x,a))
   20 continue
      return
      end
 
 
      subroutine legend(title)
      character *(*) title
      character *25 xstr,ystr
c
c   This routine puts a legend on the graph giving the title of the graph,
c   and the x and y ranges.  Preferably call just before endgraf.
c
      save /fngrfx/
      common /fngrfx/ xlo,xhi,ylo,yhi,npts
c
c       Build the strings for axis information.
c
      write(xstr,901) xlo,xhi
901   format('X: ',G9.2,' to ',G9.2)
      write(ystr,902) ylo,yhi
902   format('Y: ',G9.2,' to ',G9.2)
c
c       Define normalized window
c
      call setwin(0.0,1.0,0.0,1.0)
      call newvw2
c
c       Put up the legend
c
      call inqchr(width,height)
      call move2a(2.0*width,1.0-2.0*height)
      call text(title)
      call move2a(2.0*width,1.0-3.0*height)
      call text(xstr)
      call move2a(2.0*width,1.0-4.0*height)
      call text(ystr)
c
c       Restore the original window
c
      call setwin(xlo,xhi,ylo,yhi)
      call newvw2
      return
      end
 
 
      subroutine endgraf
C
C   This routine causes the graph to be displayed.  It then waits till
C   the user hits return, and restores the terminal to text mode.
C
C       Now bring screen up to date.
C
      call makcur
C
C  exit -- Wait for user to hit return, then finish up.
C
      call inqinu(inptu)
  500 read(inptu,501) idum
  501 format(a1)
 
      call finit
 
      end

C COREGFX.FOR:
C			    CORE GRAPHICS PACKAGE
C
C This file contains system-independent part of the CORE graphics package.  See
C the COREGFX documentation for explanations of the calling sequences.
C
C Originally written by Peter Kiernan, Spring 1985, based on S. Harrington,
C Computer Graphics, a Programming Approach.  Modified by R. Moniot.
C
C                         CORE Version 1.1
C                           March, 1991
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
C
C  System-specific code is in files VMS.FOR and UNIX.F.  Compile for
C  VMS systems with the command:
C
C         $ FORTRAN CORE+VMS
C
C  then link user programs with CORE.OBJ
C
C  The routines in this file are in library order: each routine occurs
C  ahead of all modules which it references.
C
C  See FUNGRAF.FOR for a set of higher-level applications routines using
C  the CORE library.
C
C  All routines in this file conform to ANSI FORTRAN-77.
C  The ASCII character set is assumed where control characters are needed.
C
      SUBROUTINE INIT(NDVICE,IBAUDR)
C.............................................................................
C.     SUBROUTINE INIT ASSIGNS THE PROPER VALUES TO THE VARIABLES WHICH      .
C.     SPECIFY THE DIMENSIONS OF THE OUTPUT DEVICE BEING USED.               .
C.
C.   CALLING SEQUENCE:
C.         CALL INIT(NDVICE,IBAUDR)
C.
C.    WHERE NDVICE CONTAINS THE NAME OF THE PLOTTING DEVICE.  NAMES SUPPORTED
C.    ARE:  'VT52', 'V100' (VT-100), 'PRIN' (PRINTER), 'GIGI', '4010'
C.    AND 'DUMB' (DUMB TERMINAL).
C     NOTE THAT NDVICE MUST BE OF TYPE CHARACTER*4
C
C     IBAUDR CONTAINS THE DEVICE OUTPUT SPEED (BAUD RATE).  BAUD RATE CAN
C     BE GOTTEN BY PRIOR CALL TO GETSPD.  CORRECT BAUD RATE MUST BE SPECIFIED
C     IF SUBROUTINE GFWAIT IS TO BE USED.
C.............................................................................
C
C
      CHARACTER *4 NDVICE,NDEV
      INTEGER IBAUDR,IBAUD
      INTEGER WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT,I
      INTEGER FREE,DFSIZ
      REAL DFPX,DFPY
      LOGICAL ONFLAG
      REAL XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
      LOGICAL PFLAG
      INTEGER CNTIN,CNTOUT
      REAL XS(4),YS(4)
      LOGICAL FULL
C  MAXPTS IS THE SIZE OF DISPLAY FILE
      INTEGER MAXPTS
      PARAMETER (MAXPTS=5000)
C  ENCOURAGE LINKER TO INCLUDE BLOCK DATA MODULE DEFLTS
      EXTERNAL DEFLTS
C
C
      SAVE /GFXA/
      COMMON/GFXA/WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
      SAVE /GFXSPD/
      COMMON/GFXSPD/IBAUD
      SAVE /GFXE/
      COMMON/GFXE/FREE,DFSIZ,FULL
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
      SAVE /GFXGG/
      COMMON/GFXGG/ONFLAG
      SAVE /GFXTXT/
      COMMON/GFXTXT/ XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
      SAVE /GFXCLP/
      COMMON/GFXCLP/ PFLAG,CNTIN,CNTOUT,XS,YS
C
C
      NDEV = NDVICE
      IBAUD = IBAUDR
C
C
      DFSIZ = MAXPTS

      IF(NDEV.EQ.'VT52' .OR. NDEV.EQ.'V100') THEN
C
C      ---IF USING A VT100 ASSIGN THE FOLLOWING---
C
             WEND=80
             HEND=22
             WSTRT=1
             HSTRT=1
C
             CHARW=1./(WEND-WSTRT)
             CHARH=1./(HEND-HSTRT)
             CHRSEP=0.0
C
       ELSE IF(NDEV.EQ.'GIGI') THEN
C
C	-- INITIALIZE GIGI OR OTHER DEC REGIS DEVICE HERE
C
             WEND=767
             HEND=479
             WSTRT=0
             HSTRT=0
             ONFLAG=.FALSE.
C
             CHARW=1./84.
             CHARH=1./24.
             CHRSEP=0.0
       ELSE IF(NDEV.EQ.'4010') THEN
C
C	-- Tektronix 4010 protocol.
C
             WEND=1023
             HEND=780
             WSTRT=0
             HSTRT=0
C
             CHRSEP=0.0
CC		the following values match "real" tek 4010
CC             CHARW=1./72.
CC             CHARH=1./34.
CC             the following values match MS-Kermit emulation on IBM PC.
             CHARW=1./80.
             CHARH=1./43.
       ELSE
C
C     Remaining possible devices are PRIN and DUMB.
C
            IF(NDEV.NE.'PRIN' .AND. NDEV .NE. 'DUMB') THEN
               CALL GFXERR('UNKNOWN DEVICE CODE: ASSUMED DUMB')
               NDEV = 'DUMB'
            ENDIF
C      ---IF USING A PRINTER ASSIGN FOLLOWING---
C
            WSTRT=1
            HSTRT=1
            IF(NDEV .EQ. 'DUMB') THEN
C                       DUMB TERMINAL IS 80 COLS WIDE BY 24 ROWS HIGH
C                       LEAVE LAST COL BLANK TO AVOID AUTO CR/LF's
               WEND=79
               HEND=23
            ELSE
C                       PRINTER IS 130 COLS WIDE BY 50 COLS HIGH
               WEND=130
               HEND=55
            ENDIF
C
            CHARW=1./(WEND-WSTRT)
            CHARH=1./(HEND-HSTRT)
            CHRSEP=0.0
C
         ENDIF
C		End of device-dependent initialization section
C
C
C      --- DETERMINE WIDTH AND HEIGHT---
C 
      WIDTH=WEND-WSTRT
      HEIGHT=HEND-HSTRT
C
C  INITIALIZE DISPLAY FILE VARIABLES
C
      FREE=1
      FULL = .FALSE.
      DFPX=0.0
      DFPY=0.0
C
      CALL NWFRM
C
C
C.     ASSIGN THE PROPER VALUES TO THE LINE DRAWING VARIABLES.
C
      CALL STSTYL(1)
      CALL CHARUP(0.,1.)
C
C.     INITIALIZE THE TRANSFORMATION MATRIX.
C
      CALL IDENT
C
C  INITIALIZE VIEWING PARAMETERS
C
      CALL SETVWP(0.,1.,0.,1.)
      CALL SETWIN(0.,1.,0.,1.)
      CALL NEWVW2
      DO 200 I=1,4
        XS(I)=0
        YS(I)=0
200   CONTINUE
      PFLAG=.FALSE.
      RETURN
      END
      SUBROUTINE SETGFU(GFUNIT)
C Sets the graphic output unit number to GFUNIT
C Default value is defined in BLOCK DATA DEFLTS module
      INTEGER GFUNIT
      INTEGER GFXU,ERRU,INPTU
      SAVE /GFXIO/
      COMMON/GFXIO/ GFXU,ERRU,INPTU
      GFXU = GFUNIT
      RETURN
      END
      SUBROUTINE SETERU(ERUNIT)
C Sets the graphic error output unit number to ERUNIT
C Default value is defined in BLOCK DATA DEFLTS module
      INTEGER ERUNIT
      INTEGER GFXU,ERRU,INPTU
      SAVE /GFXIO/
      COMMON/GFXIO/ GFXU,ERRU,INPTU
      ERRU = ERUNIT
      RETURN
      END
      SUBROUTINE SETINU(INUNIT)
C Sets the interactive input unit number to INUNIT
C Default value is defined in BLOCK DATA DEFLTS module
      INTEGER INUNIT
      INTEGER GFXU,ERRU,INPTU
      SAVE /GFXIO/
      COMMON/GFXIO/ GFXU,ERRU,INPTU
      INPTU = INUNIT
      RETURN
      END
      SUBROUTINE INQGFU(GFUNIT)
C Returns current value of graphic output unit number.
      INTEGER GFUNIT
      INTEGER GFXU,ERRU,INPTU
      SAVE /GFXIO/
      COMMON/GFXIO/ GFXU,ERRU,INPTU
      GFUNIT = GFXU
      RETURN
      END
      SUBROUTINE INQERU(ERUNIT)
C Returns current value of error-logging unit.
      INTEGER ERUNIT
      INTEGER GFXU,ERRU,INPTU
      SAVE /GFXIO/
      COMMON/GFXIO/ GFXU,ERRU,INPTU
      ERUNIT = ERRU
      RETURN
      END
      SUBROUTINE INQINU(INUNIT)
C  Sets INUNIT to current interactive input unit number.
      INTEGER INUNIT
      INTEGER GFXU,ERRU,INPTU
      SAVE /GFXIO/
      COMMON/GFXIO/ GFXU,ERRU,INPTU
      INUNIT = INPTU
      RETURN
      END
      SUBROUTINE POLY2T(AX,AY,N)
C.............................................................................
C.     SUBROUTINE POLY2T APPLIES THE TRANSFORMATION MATRIX TO A
C.     RELATIVE POLYGON BEFORE IT IS ENTERED INTO THE DISPLAY FILE.
C.............................................................................
C
      INTEGER N,I
      REAL DFPX,DFPY,AX,AY,TMPX,TMPY,NEWX,NEWY
C
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
C
      DIMENSION AX(32),AY(32)
C
      IF((N.GT.31).OR.(N.LT.3))THEN
C
         CALL GFXERR('POLYGON SIZE ERROR-- SKIPPED')
         RETURN
C
      ENDIF
C
      CALL DOTRNS(AX(1),AY(1),NEWX,NEWY)
      DFPX=DFPX+NEWX
      DFPY=DFPY+NEWY
      TMPX=DFPX
      TMPY=DFPY
C
      CALL DFENTR(N)
C
      DO 100 I=2,N
C
         CALL LINE2T(AX(I),AY(I))
C 
 100  CONTINUE
C
      CALL LINE2A(TMPX,TMPY)
C
      RETURN
      END
      SUBROUTINE POLY2A(AX,AY,N)
C..............................................................................
C.     SUBROUTINE POLY2A ENTERS INSTRUCTIONS INTO THE DISPLAY FILE FOR AN    .
C.     ABSOLUTE POLYGON.                                                      .
C..............................................................................
C
      INTEGER N,I
      REAL DFPX,DFPY,AX,AY
C
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
C
      DIMENSION AX(32),AY(32)
C
      IF((N.GT.31).OR.(N.LT.3))THEN
C
         CALL GFXERR('POLYGON SIZE ERROR-- SKIPPED')
         RETURN
C
      ENDIF
C
      DFPX=AX(N)
      DFPY=AY(N)
C
      CALL DFENTR(N)
C
      DO 100 I=1,N
C
         CALL LINE2A(AX(I),AY(I))
C 
 100   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE POLY2R(AX,AY,N)
C.............................................................................
C.     SUBROUTINE POLY2R ENTERS INSTRUCTIONS INTO THE DISPLAY FILE FOR A    .
C.     RELATIVE POLYGON.                                                     .
C.............................................................................
C
      INTEGER N,I
      REAL DFPX,DFPY,AX,AY,TMPX,TMPY
C
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
C
      DIMENSION AX(32),AY(32)
C
      IF((N.GT.31).OR.(N.LT.3))THEN
C
         CALL GFXERR('POLYGON SIZE ERROR-- SKIPPED')
         RETURN
C
      ENDIF
C
      DFPX=DFPX+AX(1)
      DFPY=DFPY+AY(1)
      TMPX=DFPX
      TMPY=DFPY
C
      CALL DFENTR(N)
C
      DO 100 I=2,N
C
         CALL LINE2R(AX(I),AY(I))
C 
 100   CONTINUE
C
      CALL LINE2A(TMPX,TMPY)
C
      RETURN
      END
C
      SUBROUTINE TEXT(STRING)
C.........................................................................
C   SUBROUTINE TEXT PLACES INSTRUCTIONS FOR PRINTING A STRING INTO
C   THE DISPLAY FILE
C.........................................................................
      CHARACTER* (*) STRING
      INTEGER LENGTH,I,OP
      REAL DFPX,DFPY,X,Y,XSTEP,YSTEP
      REAL XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
      REAL VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
      SAVE /GFXVW/
      COMMON/GFXVW/ VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
      SAVE /GFXTXT/
      COMMON/GFXTXT/ XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
C
      LENGTH=LEN(STRING)
      X=DFPX
      Y=DFPY
C  -- The following adjustment for window/viewport coords is not in Harrington.
      XSTEP=XCHRSP/WSX
      YSTEP=YCHRSP/WSY

      DO 100 I=1,LENGTH
        OP=ICHAR(STRING(I:I))
        CALL DFENTR(OP)
        DFPX=X+I*XSTEP
        DFPY=Y+I*YSTEP
100   CONTINUE
C
C  RESTORE ORIGINAL PEN POSITION
C
      CALL MOVE2A(X,Y)
      RETURN
      END
      SUBROUTINE TEXT2A(X,Y)
C........................................................................
C   SUBROUTINE TEXT2A IS USED FOR INTERACTIVE APPLICATIONS.  AFTER THE
C   DRAWING COMMANDS FOR A PICTURE HAVE BEEN GIVEN, THE PROGRAM CAN CALL
C   TEXT2A TO POSITION THE CURSOR AT LOCATION (X,Y) IN USER COORDINATES
C   TO BEGIN INTERACTIVE I/O WITH THE USER.  THIS SUBROUTINE MAKES SURE
C   THAT THE PICTURE IS UP TO DATE, THEN MOVES TO (X,Y) AND PUTS THE
C   TERMINAL INTO ALPHA MODE.
C.........................................................................
      REAL X,Y
C
      CALL MOVE2A(X,Y)
      CALL MAKCUR
      CALL ALPHMD
      CALL ACURSR
      RETURN
      END
C
      SUBROUTINE TEXT2R(DX,DY)
C
C  SUBROUTINE TEXT2R IS THE SAME AS TEXT2A EXCEPT THAT THE MOVE IS
C  RELATIVE TO THE CURRENT CURSOR POSITION.
C
      REAL DX,DY
C
      CALL MOVE2R(DX,DY)
      CALL MAKCUR
      CALL ALPHMD
      CALL ACURSR
      RETURN
      END
      SUBROUTINE MOVE2T(DX,DY)
C.............................................................................
C.     SUBROUTINE MOVE2T APPLIES THE TRANSFORMATION MATRIX TO A POINT BEFORE .
C.     IT IS ENTERED INTO THE DISPLAY FILE.                                  .
C.............................................................................
C
      REAL DX,DY,NWX,NWY
C
      CALL DOTRNS(DX,DY,NWX,NWY)
C
      CALL MOVE2R(NWX,NWY)
C
      RETURN
      END
C
      SUBROUTINE MOVE2A(X,Y)
C..............................................................................
C.     SUBROUTINE MOVE2A ENTERS A MOVE INSTRUCTION INTO THE DISPLAY FILE.     .
C..............................................................................
C
      REAL X,Y,DFPX,DFPY
C
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
C
      DFPX=X
      DFPY=Y
C
      CALL DFENTR(1)
C
      RETURN
      END
      SUBROUTINE MOVE2R(DX,DY)
C..............................................................................
C.     SUBROUTINE MOVE2R ENTERS AN INSTRUCTION FOR A MOVE RELATIVE TO         .
C.     THE CURRENT PEN POSITION INTO THE DISPLAY FILE.                        .
C..............................................................................
C
      REAL DX,DY,DFPX,DFPY
C
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
C
      DFPX=DFPX+DX
      DFPY=DFPY+DY
C
      CALL DFENTR(1)
C
      RETURN
      END
C
C
C
      SUBROUTINE LINE2T(DX,DY)
C.............................................................................
C.    SUBROUTINE LINE2T APPLIES THE TRANSFORMATION MATRIX TO THE LINE BEFORE .
C.    IT IS ENTERED INTO THE DISPLAY FILE.                                   .
C.............................................................................
C
      REAL DX,DY,NWX,NWY
C
      CALL DOTRNS(DX,DY,NWX,NWY)
C
      CALL LINE2R(NWX,NWY)
C
      RETURN
      END
      SUBROUTINE LINE2A(X,Y)
C..............................................................................
C.     SUBROUTINE LINE2A ENTERS A LINE DRAWING INSTRUCTION INTO THE DISPLAY   .
C.     FILE.                                                                  .
C..............................................................................
C
      REAL X,Y,DFPX,DFPY
C
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
C
      DFPX=X
      DFPY=Y
C
      CALL DFENTR(2)
C
      RETURN
      END
C
      SUBROUTINE LINE2R(DX,DY)
C..............................................................................
C.     SUBROUTINE LINE2R ENTERS AN INSTRUCTION FOR DRAWING A LINE RELATIVE TO .
C.     THE CURRENT PEN POSITION INTO THE DISPLAY FILE.                        .
C..............................................................................
C
      REAL DX,DY,DFPX,DFPY
C
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
C
      DFPX=DFPX+DX
      DFPY=DFPY+DY
C
      CALL DFENTR(2)
C
      RETURN
      END
      SUBROUTINE STSTYL(LNSTYL)
C..............................................................................
C.     SUBROUTINE STSTYL SETS THE CHARACTER USED FOR THE LINE STYLE.         .
C..............................................................................
C
      INTEGER LNSTYL
C
      CALL DFENTR(-LNSTYL)
C
      RETURN
      END
C
      SUBROUTINE CHRSPC(SPACE)
C........................................................................
C    SUBROUTINE CHRSPC SETS THE SPACING BETWEEN CHARACTERS
C........................................................................
      REAL SPACE,XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
      SAVE /GFXTXT/
      COMMON/GFXTXT/ XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
C
      CHRSEP=SPACE
      CALL CHARUP(-YCHRSP,XCHRSP)
      RETURN
      END
      SUBROUTINE CHARUP(DX,DY)
C.......................................................................
C    SUBROUTINE CHARUP SETS THE "UP" DIRECTION FOR CHARACTERS
C.......................................................................
      REAL DX,DY,S,S1,S2,XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
      SAVE /GFXTXT/
      COMMON/GFXTXT/ XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
C
      S = DX*DX + DY*DY
      IF(S .LT. 1.E-6) THEN
        CALL GFXERR('ERROR--BAD CHARUP DIRECTION: IGNORED')
        RETURN
      ENDIF
      S1 = ABS(CHARW*DY)+ABS(CHARH*DX)
      S2 = S1*(1.0+CHRSEP)/S
      XCHRSP=DY*S2
      YCHRSP=-DX*S2
      RETURN
      END
      SUBROUTINE INQCHR(WIDTH,HEIGHT)
C
C  RETURNS CHARACTER WIDTH AND HEIGHT IN NORMALIZED UNITS (0 TO 1).
C  WILL NOT WORK BEFORE INIT IS CALLED, SINCE INIT SETS THESE UP.
C
      REAL WIDTH,HEIGHT
      REAL XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
      SAVE /GFXTXT/
      COMMON/GFXTXT/ XCHRSP,YCHRSP,CHARW,CHARH,CHRSEP
      WIDTH=CHARW
      HEIGHT=CHARH
      RETURN
      END
      SUBROUTINE INQSPD(NBAUD)
C
C  RETURNS DEVICE BAUD RATE.  THIS WORKS ONLY AFTER INIT IS CALLED,
C  SINCE INIT SETS THE COMMON VARIABLES.
C
      INTEGER NBAUD,IBAUD
      SAVE /GFXSPD/
      COMMON/GFXSPD/IBAUD
      NBAUD = IBAUD
      RETURN
      END
C
      SUBROUTINE IDENT
C..............................................................................
C.     SUBROUTINE IDENT SETS IDENTITY TRANSFORMATION PARAMETERS.              .
C..............................................................................
C
      REAL SCLX,SCLY,ANGL,TRNX,TRNY
      LOGICAL NEWTRN
C
      SAVE /GFXTRN/
      COMMON/GFXTRN/SCLX,SCLY,ANGL,TRNX,TRNY,NEWTRN
C
        SCLX=1.0
        SCLY=1.0
        ANGL=0.0
        TRNX=0.0
        TRNY=0.0
        NEWTRN=.FALSE.
      RETURN
      END
C
C
      SUBROUTINE TRNSLT(TX,TY)
C..............................................................................
C.     SUBROUTINE TRNSLT SETS THE TRANSLATION PARAMETERS.                     .
C..............................................................................
      REAL TX,TY
C
      REAL SCLX,SCLY,ANGL,TRNX,TRNY
      LOGICAL NEWTRN
      SAVE /GFXTRN/
      COMMON/GFXTRN/SCLX,SCLY,ANGL,TRNX,TRNY,NEWTRN
C
        TRNX=TX
        TRNY=TY
        NEWTRN=.TRUE.
C
      RETURN
      END
C
      SUBROUTINE SCALE(SX,SY)
C..............................................................................
C.     SUBROUTINE SCALE SETS THE SCALING PARAMETERS.
C..............................................................................
C
      REAL SX,SY
      REAL SCLX,SCLY,ANGL,TRNX,TRNY
      LOGICAL NEWTRN
C
      SAVE /GFXTRN/
      COMMON/GFXTRN/SCLX,SCLY,ANGL,TRNX,TRNY,NEWTRN
C
        SCLX=SX
        SCLY=SY
        NEWTRN=.TRUE.
C
      RETURN
      END
C
      SUBROUTINE ROTATE(A)
C..............................................................................
C.     SUBROUTINE ROTATE SETS THE ROTATION ANGLE.                             .
C..............................................................................
C
      REAL A
C
      REAL SCLX,SCLY,ANGL,TRNX,TRNY
      LOGICAL NEWTRN
      SAVE /GFXTRN/
      COMMON/GFXTRN/SCLX,SCLY,ANGL,TRNX,TRNY,NEWTRN
C
        ANGL=A
C
        NEWTRN=.TRUE.
      RETURN
      END
      SUBROUTINE ALPHMD
C.....................................................................
C   SUBROUTINE ALPHMD PUTS THE TERMINAL INTO ALPHANUMERIC, I.E. NON-
C   GRAPHIC MODE.
C
C   FOR MOST TERMINALS, THERE IS ONLY ONE ALPHA MODE.  HOWEVER, FOR
C   TEKTRONIX EMULATORS SUCH AS VISUAL 550, THERE ARE TWO ALPHA MODES:
C   ALPHAGRAPHICS WHICH EMULATES THE TEKTRONIX ALPHA MODE AND KEEPS THE GRAPHIC
C   DISPLAY ON SCREEN, (SELECTED BY THIS SUBROUTINE), AND ALPHANUMERIC WHICH
C   GOES OUT OF TEKTRONIX EMULATION MODE ENTIRELY (SELECTED BY FINIT).
C.....................................................................
      CHARACTER *4 NDEV
      INTEGER US
      PARAMETER (US = 31)
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
      IF(NDEV.EQ.'GIGI')THEN
          CALL GGOFF
      ELSE IF(NDEV.EQ.'4010')THEN
          CALL PUTSTR( CHAR(US) )
      ENDIF
      RETURN
      END
      SUBROUTINE SETVWP(XL,XH,YL,YH)
C
C  SUBROUTINE SETVWP SETS THE VIEWPORT BOUNDARIES.  LOWER LEFT CORNER
C  WILL BE (XL,YL) AND UPPER RIGHT CORNER WILL BE (XH,YH).  COORDINATES
C  ARE GIVEN IN NORMALIZED SCREEN COORDINATES, I.E. WHOLE SCREEN IS
C  FROM 0 TO 1 IN EACH DIRECTION.
C
      REAL XL,XH,YL,YH
      REAL VXLHLD,VYLHLD,VXHHLD,VYHHLD
      SAVE /GFXVH/
      COMMON/GFXVH/ VXLHLD,VYLHLD,VXHHLD,VYHHLD
      IF(XL .GE. XH .OR. YL .GE. YH) THEN
        CALL GFXERR('ERROR--BAD VIEWPORT SPECS: IGNORED')
        RETURN
      ENDIF
C
C  HOLD THE NEW VALUES TILL NEWVW2 IS CALLED
      VXLHLD=XL
      VYLHLD=YL
      VXHHLD=XH
      VYHHLD=YH
      RETURN
      END
C
      SUBROUTINE SETWIN(XL,XH,YL,YH)
C
C  SUBROUTINE SETWIN SETS THE WINDOW BOUNDARIES.  THE LOWER LEFT CORNER
C  OF WINDOW WILL BE (XL,YL) AND THE UPPER RIGHT CORNER WILL BE (XH,YH).
C  COORDINATES ARE IN USER OR WORLD COORDINATES, I.E. ARBITRARY UNITS.
C
      REAL XL,XH,YL,YH
      REAL WXLHLD,WYLHLD,WXHHLD,WYHHLD
      SAVE /GFXWH/
      COMMON/GFXWH/ WXLHLD,WYLHLD,WXHHLD,WYHHLD
      IF(XL.GE.XH .OR. YL.GE.YH) THEN
        CALL GFXERR('ERROR--BAD WINDOW SPECS: IGNORED')
        RETURN
      ENDIF
      WXLHLD=XL
      WYLHLD=YL
      WXHHLD=XH
      WYHHLD=YH
      RETURN
      END
      SUBROUTINE NWFRM
C..............................................................................
C.     SUBROUTINE NWFRM SETS FLAG TO INDICATE FRAME BUFFER MUST BE CLEARED.   .
C..............................................................................
C
      INTEGER EFLAG
C
      SAVE /GFXH/
      COMMON/GFXH/EFLAG
C
      EFLAG=1
C
      RETURN
      END
C
      SUBROUTINE NEWVW2
C
C  SUBROUTINE NEWVW2 SETS THE CLIPPING AND VIEWING PARAMETERS BASED
C  ON THE LATEST WINDOW AND VIEWPORT SPECIFICATIONS
C
      REAL VXLHLD,VYLHLD,VXHHLD,VYHHLD
      REAL WXLHLD,WYLHLD,WXHHLD,WYHHLD
      REAL VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
      SAVE /GFXVH/
      COMMON/GFXVH/ VXLHLD,VYLHLD,VXHHLD,VYHHLD
      SAVE /GFXWH/
      COMMON/GFXWH/ WXLHLD,WYLHLD,WXHHLD,WYHHLD
      SAVE /GFXVW/
      COMMON/GFXVW/ VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
C
      WXL=WXLHLD
      WYL=WYLHLD
      WXH=WXHHLD
      WYH=WYHHLD
C
      VXL=VXLHLD
      VYL=VYLHLD
      VXH=VXHHLD
      VYH=VYHHLD
C
      WSX=(VXH-VXL)/(WXH-WXL)
      WSY=(VYH-VYL)/(WYH-WYL)
      RETURN
      END
      SUBROUTINE GFWAIT(TIME)
C
C  SUBROUTINE WAIT CAUSES A REAL-TIME PAUSE IN TRANSMISSION BY SENDING
C  ENOUGH STALL CHARACTERS FOR THE REQUIRED DELAY.  ARGUMENT TIME IS
C  IN SECONDS.
      REAL TIME
      INTEGER IBAUD,NCHARS,I
      SAVE /GFXSPD/
      COMMON/GFXSPD/IBAUD
      INTEGER SYN
      PARAMETER (SYN = 22)
C
      NCHARS = INT( (TIME*IBAUD)/10.0 )
      DO 50 I=1,NCHARS
        CALL PUTSTR( CHAR(SYN) )
50      CONTINUE
      RETURN
      END
      SUBROUTINE FINIT
C.......................................................................
C   SUBROUTINE FINIT CLOSES ANY DEVICES NECESSARY, AND PUTS DEVICE
C   BACK INTO NORMAL MODE
C......................................................................
      CHARACTER *4 NDEV
      INTEGER US,CAN
      PARAMETER (US = 31, CAN = 24)
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
      IF(NDEV.EQ.'GIGI')THEN
          CALL GGOFF
      ELSE IF(NDEV.EQ.'4010')THEN
          CALL PUTSTR( CHAR(US) // CHAR(CAN) )
      ENDIF
      RETURN
      END
      SUBROUTINE MAKCUR
C..............................................................................
C.     SUBROUTINE MAKCUR DISPLAYS THE CURRENT DISPLAY FILE.                   .
C..............................................................................
C
      INTEGER FREE,DFSIZ,EFLAG
C
      CHARACTER *4 NDEV
      LOGICAL FULL
      SAVE /GFXE/
      COMMON/GFXE/FREE,DFSIZ,FULL
      SAVE /GFXH/
      COMMON/GFXH/EFLAG
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
C
      IF(NDEV .EQ. 'GIGI') CALL GGON
C
      IF(EFLAG.EQ.1)THEN
        CALL CLEAR
        EFLAG=0
      ENDIF
C
      CALL BLDTRN
C
      IF(FREE.GT.1) CALL INTPRT(1,FREE-1)
C
      IF(NDEV.EQ.'PRIN' .OR. NDEV .EQ. 'DUMB')CALL DISPLY
C
      FREE=1
C
      RETURN
      END
      SUBROUTINE DFENTR(OP)
C..............................................................................
C.     SUBROUTINE DFENTER FORMS AN INSTRUCTION AND PUTS IT IN THE DISPLAY     .
C.     FILE .                                                                 .
C..............................................................................
C
      INTEGER OP
      REAL DFPX,DFPY
C
      SAVE /GFXC/
      COMMON/GFXC/DFPX,DFPY
C
      IF(OP.LT.1) THEN
        CALL PUTPT(OP,DFPX,DFPY)
      ELSE
        CALL CLIP(OP,DFPX,DFPY)
      ENDIF
C
      RETURN
      END
      SUBROUTINE CLIP(OP,X,Y)
C
C  SUBROUTINE CLIP DECIDES BETWEEN CLIPPING POLYGONS AND OTHER PRIMITIVES
C
      INTEGER OP
      REAL X,Y
      LOGICAL PFLAG
      INTEGER CNTIN,CNTOUT,I
      REAL XS(4),YS(4)
      SAVE /GFXCLP/
      COMMON/GFXCLP/ PFLAG,CNTIN,CNTOUT,XS,YS
C
      IF(PFLAG) THEN
        CALL POLCLP(OP,X,Y)
      ELSE IF(OP .GT. 2 .AND. OP .LT. 32) THEN
        PFLAG=.TRUE.
        CNTIN=OP
        CNTOUT=0
        DO 10 I=1,4
          XS(I)=X
          YS(I)=Y
10      CONTINUE
      ELSE
        CALL CLIPL(OP,X,Y)
      ENDIF
      RETURN
      END
C
      SUBROUTINE POLCLP(OP,X,Y)
C
C  SUBROUTINE POLCLP HANDLES CLIPPING POLYGON SIDES AND FINISHING
C  CLIPPING OF A POLYGON
C
      INTEGER OP
      REAL X,Y
      LOGICAL PFLAG
      INTEGER CNTIN,CNTOUT,I
      REAL XS(4),YS(4)
      INTEGER OPTEMP(32)
      REAL XTEMP(32),YTEMP(32)
      SAVE /GFXCLP/
      COMMON/GFXCLP/ PFLAG,CNTIN,CNTOUT,XS,YS
      SAVE /GFXTMP/
      COMMON/GFXTMP/ OPTEMP,XTEMP,YTEMP
C
C  CLIP THE NEXT SIDE
C
      CNTIN=CNTIN-1
      CALL CLIPL(OP,X,Y)
      IF(CNTIN.NE.0) RETURN
C
C  CLOSE THE CLIPPED POLYGON.  N.B. Harrington has XTEMP(1),YTEMP(1) here
C
      IF(CNTOUT.GT.0) CALL CLIPL(2,XTEMP(CNTOUT),YTEMP(CNTOUT))
      PFLAG=.FALSE.
      CNTOUT=CNTOUT-1
      IF(CNTOUT.LT.3) RETURN
C
C  COPY CLIPPED POLYGON FROM TEMPORARY FILE TO DISPLAY FILE
C
      IF(CNTOUT.LT.32) THEN
        CALL VWTRAN(CNTOUT,XTEMP(CNTOUT),YTEMP(CNTOUT))
        DO 100 I=1,CNTOUT
          CALL VWTRAN(OPTEMP(I),XTEMP(I),YTEMP(I))
100       CONTINUE
      ELSE
          CALL GFXERR(
     $          'ERROR--CLIPPED POLYGON HAS TOO MANY SIDES: DELETED')
      ENDIF
      RETURN
      END
      SUBROUTINE CLIPL(OP,X,Y)
C
C  SUBROUTINE CLIPL CLIPS THE GIVEN POINT AGAINST THE LEFT SIDE OF WINDOW.
C
      INTEGER OP
      REAL X,Y
      LOGICAL PFLAG
      INTEGER CNTIN,CNTOUT
      REAL XS(4),YS(4)
      REAL VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
      SAVE /GFXCLP/
      COMMON/GFXCLP/ PFLAG,CNTIN,CNTOUT,XS,YS
      SAVE /GFXVW/
      COMMON/GFXVW/ VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
C
C  CASE OF DRAWING FROM OUTSIDE IN
C
      IF(X.GE.WXL .AND. XS(1).LT.WXL) 
     %    CALL CLIPR( 1,WXL,(Y-YS(1))*(WXL-X)/(X-XS(1))+Y)
C
C  CASE OF DRAWING FROM INSIDE OUT
      IF(X.LE.WXL .AND. XS(1).GT.WXL) THEN
        IF(OP .LT. 32) THEN
           CALL CLIPR( OP,WXL,(Y-YS(1))*(WXL-X)/(X-XS(1))+Y)
        ELSE
           CALL CLIPR( 1,WXL,(Y-YS(1))*(WXL-X)/(X-XS(1))+Y)
        ENDIF
      ENDIF
C
C  REMEMBER POINT TO SERVE AS ENDPOINT FOR NEXT LINE SEGMENT
C
      XS(1)=X
      YS(1)=Y
C
C  CASE OF POINT INSIDE WINDOW
C
      IF(X.GE.WXL) CALL CLIPR(OP,X,Y)
C
      RETURN
      END
C
      SUBROUTINE CLIPR(OP,X,Y)
C
C  SUBROUTINE CLIPR CLIPS THE GIVEN POINT AGAINST THE RIGHT SIDE OF WINDOW.
C
      INTEGER OP
      REAL X,Y
      LOGICAL PFLAG
      INTEGER CNTIN,CNTOUT
      REAL XS(4),YS(4)
      REAL VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
      SAVE /GFXCLP/
      COMMON/GFXCLP/ PFLAG,CNTIN,CNTOUT,XS,YS
      SAVE /GFXVW/
      COMMON/GFXVW/ VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
C
C  CASE OF DRAWING FROM OUTSIDE IN
C
      IF(X.LE.WXH .AND. XS(2).GT.WXH) 
     %    CALL CLIPB( 1,WXH,(Y-YS(2))*(WXH-X)/(X-XS(2))+Y)
C
C  CASE OF DRAWING FROM INSIDE OUT
      IF(X.GE.WXH .AND. XS(2).LT.WXH) THEN
        IF(OP .LT. 32) THEN
          CALL CLIPB( OP,WXH,(Y-YS(2))*(WXH-X)/(X-XS(2))+Y)
        ELSE
          CALL CLIPB( 1,WXH,(Y-YS(2))*(WXH-X)/(X-XS(2))+Y)
        ENDIF
      ENDIF
C
C  REMEMBER POINT TO SERVE AS ENDPOINT FOR NEXT LINE SEGMENT
C
      XS(2)=X
      YS(2)=Y
C
C  CASE OF POINT INSIDE WINDOW
C
      IF(X.LE.WXH) CALL CLIPB(OP,X,Y)
C
      RETURN
      END
C
      SUBROUTINE CLIPB(OP,X,Y)
C
C  SUBROUTINE CLIPB CLIPS THE GIVEN POINT AGAINST THE BOTTOM OF WINDOW.
C
      INTEGER OP
      REAL X,Y
      LOGICAL PFLAG
      INTEGER CNTIN,CNTOUT
      REAL XS(4),YS(4)
      REAL VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
      SAVE /GFXCLP/
      COMMON/GFXCLP/ PFLAG,CNTIN,CNTOUT,XS,YS
      SAVE /GFXVW/
      COMMON/GFXVW/ VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
C
C  CASE OF DRAWING FROM OUTSIDE IN
C
      IF(Y.GE.WYL .AND. YS(3).LT.WYL) 
     %    CALL CLIPT(1,(X-XS(3))*(WYL-Y)/(Y-YS(3))+X,WYL)
C
C  CASE OF DRAWING FROM INSIDE OUT
      IF(Y.LE.WYL .AND. YS(3).GT.WYL) THEN
        IF(OP .LT. 32) THEN
          CALL CLIPT(OP,(X-XS(3))*(WYL-Y)/(Y-YS(3))+X,WYL)
        ELSE
          CALL CLIPT(1,(X-XS(3))*(WYL-Y)/(Y-YS(3))+X,WYL)
        ENDIF
      ENDIF
C
C  REMEMBER POINT TO SERVE AS ENDPOINT FOR NEYT LINE SEGMENT
C
      XS(3)=X
      YS(3)=Y
C
C  CASE OF POINT INSIDE WINDOW
C
      IF(Y.GE.WYL) CALL CLIPT(OP,X,Y)
C
      RETURN
      END
C
      SUBROUTINE CLIPT(OP,X,Y)
C
C  SUBROUTINE CLIPT CLIPS THE GIVEN POINT AGAINST THE TOP OF WINDOW.
C
      INTEGER OP
      REAL X,Y
      LOGICAL PFLAG
      INTEGER CNTIN,CNTOUT
      REAL XS(4),YS(4)
      REAL VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
      SAVE /GFXCLP/
      COMMON/GFXCLP/ PFLAG,CNTIN,CNTOUT,XS,YS
      SAVE /GFXVW/
      COMMON/GFXVW/ VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
C
C  CASE OF DRAWING FROM OUTSIDE IN
C
      IF(Y.LE.WYH .AND. YS(4).GT.WYH) 
     %    CALL SAVCLP(1,(X-XS(4))*(WYH-Y)/(Y-YS(4))+X,WYH)
C
C  CASE OF DRAWING FROM INSIDE OUT
C
      IF(Y.GE.WYH .AND. YS(4).LT.WYH) THEN
        IF(OP .LT. 32) THEN
          CALL SAVCLP(OP,(X-XS(4))*(WYH-Y)/(Y-YS(4))+X,WYH)
        ELSE
          CALL SAVCLP(1,(X-XS(4))*(WYH-Y)/(Y-YS(4))+X,WYH)
        ENDIF
      ENDIF
C
C  REMEMBER POINT TO SERVE AS ENDPOINT FOR NEYT LINE SEGMENT
C
      XS(4)=X
      YS(4)=Y
C
C  CASE OF POINT INSIDE WINDOW
C
      IF(Y.LE.WYH) CALL SAVCLP(OP,X,Y)
C
      RETURN
      END
C
      SUBROUTINE SAVCLP(OP,X,Y)
C
C  SUBROUTINE SAVCLP SAVES CLIPPED POINT.  IF POINT IS PART OF A POLYGON,
C  IT GOES INTO TEMPORARY ARRAY.  OTHERWISE IT GOES INTO DISPLAY FILE.
C
      INTEGER OP
      REAL X,Y
      LOGICAL PFLAG
      INTEGER CNTIN,CNTOUT
      REAL XS(4),YS(4)
      REAL VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
      SAVE /GFXCLP/
      COMMON/GFXCLP/ PFLAG,CNTIN,CNTOUT,XS,YS
      SAVE /GFXVW/
      COMMON/GFXVW/ VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
C
      IF(PFLAG) THEN
        CNTOUT=CNTOUT+1
        IF(CNTOUT .LE. 32) CALL PUTTMP(OP,X,Y,CNTOUT)
      ELSE
        CALL VWTRAN(OP,X,Y)
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE VWTRAN(OP,X,Y)
C
C  SUBROUTINE VWTRAN PERFORMS THE VIEWING-TRANSFORMATION TO CONVERT
C  A GIVEN POINT FROM USER COORDINATES TO NORMALIZED COORDINATES
C
      INTEGER OP
      REAL X,Y
      REAL VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
      SAVE /GFXVW/
      COMMON/GFXVW/ VXL,VYL,VXH,VYH,WXL,WYL,WXH,WYH,WSX,WSY
C
      CALL PUTPT(OP,(X-WXL)*WSX+VXL,(Y-WYL)*WSY+VYL)
      RETURN
      END
      SUBROUTINE PUTTMP(OP,X,Y,INDEX)
C
C  SUBROUTINE PUTTMP PUTS A POINT INTO THE TEMPORARY STORAGE FOR
C  POLYGON EDGES WHILE THEY ARE BEING CLIPPED
C
      INTEGER OP,INDEX
      REAL X,Y
      INTEGER OPTEMP(32)
      REAL XTEMP(32),YTEMP(32)
      SAVE /GFXTMP/
      COMMON/GFXTMP/ OPTEMP,XTEMP,YTEMP
      OPTEMP(INDEX)=OP
      XTEMP(INDEX)=X
      YTEMP(INDEX)=Y
      RETURN
      END
      SUBROUTINE PUTPT(OP,X,Y)
C.............................................................................
C.     SUBROUTINE PUTPT PLACES A FULL INSTRUCTION INTO THE DISPLAY FILE.     .
C.............................................................................
C
      INTEGER OP,FREE,DFSIZ,DFOP
      REAL X,Y,DFX,DFY
      LOGICAL FULL
      INTEGER MAXPTS
      PARAMETER (MAXPTS=5000)

      SAVE /GFXE/
      COMMON/GFXE/FREE,DFSIZ,FULL
C
      SAVE /GFXD/
      COMMON/GFXD/DFOP(MAXPTS),DFX(MAXPTS),DFY(MAXPTS)
C
      IF(FREE.EQ.DFSIZ)THEN
C
        IF(.NOT. FULL) CALL GFXERR('DISPLAY FILE FULL')
        FULL = .TRUE.
C
      ELSE
C
        DFOP(FREE)=OP
        DFX(FREE)=X
        DFY(FREE)=Y
        FREE=FREE+1
C
      ENDIF
C
      RETURN
      END
C
C
C ---- End of Display-File-Building Routines ----
C ---- Following are Display-File-Interpreting Routines ----
C
      SUBROUTINE INTPRT(START,COUNT)
C..............................................................................
C   SUBROUTINE INTPRT SCANS THE DISPLAY FILE AND PERFORMS THE INSTRUCTIONS.  .
C..............................................................................
C
      INTEGER START,COUNT,NTH,OP
      REAL X,Y
C
      DO 100 NTH=START,START+COUNT-1
C
         CALL GTTRPT(NTH,OP,X,Y)
C
           IF(OP.LT.0)THEN
C
             CALL DOSTYL(IABS(OP))
C
           ELSEIF(OP.EQ.1)THEN
C
             CALL DOMOVE(X,Y)
C
           ELSEIF(OP.EQ.2)THEN
C
             CALL DOLINE(X,Y)
C
           ELSEIF(OP.LT.32)THEN
C
             CALL DOPOLY(OP,X,Y,NTH)
C
           ELSE
             CALL DOCHAR(OP,X,Y)
           ENDIF
C 
 100   CONTINUE
C
      RETURN
      END
      SUBROUTINE GTTRPT(NTH,OP,NWX,NWY)
C..............................................................................
C.     SUBROUTINE GTTRPT RETRIEVES AND TRANSFORMS THE NTH INSTRUCTION FROM    .
C.      THE DISPLAY FILE.                                                     .
C..............................................................................
C
      REAL X,Y,NWX,NWY
      INTEGER NTH,OP
C
      CALL GETPT(NTH,OP,X,Y)
C
      IF(OP.GT.0)THEN
        CALL DOTRNS(X,Y,NWX,NWY)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE GETPT(NTH,OP,X,Y)
C..............................................................................
C.     SUBROUTINE GETPT RETRIEVES THE NTH INSTRUCTION FROM THE DISPLAY FILE   .
C..............................................................................
C
      REAL X,Y,DFX,DFY
      INTEGER NTH,OP,DFOP
      INTEGER MAXPTS

      PARAMETER (MAXPTS=5000)
C
      SAVE /GFXD/
      COMMON/GFXD/DFOP(MAXPTS),DFX(MAXPTS),DFY(MAXPTS)
C
      OP=DFOP(NTH)
      X=DFX(NTH)
      Y=DFY(NTH)
C
      RETURN
      END
      SUBROUTINE DOCHAR(OP,X,Y)
C...........................................................................
C   SUBROUTINE DOCHAR PRINTS A CHARACTER AT THE GIVEN X,Y LOCATION
C...........................................................................
      INTEGER OP
      CHARACTER CHR
      INTEGER WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      REAL X,Y,FPX,FPY
      INTEGER US
      CHARACTER APOST,QUOTE
      PARAMETER (US = 31, APOST = '''', QUOTE = '"')
      CHARACTER *4 NDEV
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
C
      SAVE /GFXA/
      COMMON/GFXA/WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      SAVE /GFXF/
      COMMON/GFXF/FPX,FPY
C
      CHR = CHAR(OP)
C
      IF(NDEV.EQ.'PRIN' .OR. NDEV .EQ. 'DUMB'
     $   .OR. NDEV.EQ.'VT52' .OR. NDEV.EQ.'V100')THEN
C
          FPX=AMAX1(FLOAT(WSTRT),AMIN1(FLOAT(WEND),X*WIDTH+WSTRT))
          FPY=AMAX1(FLOAT(HSTRT),AMIN1(FLOAT(HEND),Y*HEIGHT+HSTRT))
          CALL SETPIX(NINT(FPY),NINT(FPX),CHR)
      ELSE IF(NDEV.EQ.'GIGI')THEN
          CALL DOMOVE(X,Y)
C If character is not ' then quote it with ' otherwise use "
          IF( CHR .NE. APOST ) THEN
             CALL PUTSTR('T' // APOST // CHR // APOST)
          ELSE
             CALL PUTSTR('T' // QUOTE // CHR // QUOTE)
          ENDIF
      ELSE IF(NDEV.EQ.'4010')THEN
          CALL DOMOVE(X,Y)
          CALL PUTSTR( CHAR(US) // CHR)
      ENDIF
      RETURN
      END
      SUBROUTINE DOPOLY(OP,X,Y,NTH)
C..............................................................................
C.     SUBROUTINE DOPOLY PROCESSES A POLYGON COMMAND.                         .
C..............................................................................
C
      INTEGER OP,NTH
      REAL X,Y
C
      CALL DOMOVE(X,Y)
C
      RETURN
      END
      SUBROUTINE DOLINE(X,Y)
C..............................................................................
C.     SUBROUTINE DOLINE DRAWS A LINE.                                        .
C..............................................................................
C
      INTEGER WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT,LNCHR
      REAL X,Y,FPX,FPY,X1,Y1
      CHARACTER *4 NDEV
      CHARACTER*1 SYMBOL(6)
C
      SAVE /GFXA/
      COMMON/GFXA/WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      SAVE /GFXF/
      COMMON/GFXF/FPX,FPY
      SAVE /GFXG/
      COMMON/GFXG/LNCHR
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
C
      DATA SYMBOL/'*','+','#','X','O','.'/
C
      X1=FPX
      Y1=FPY
      FPX=AMAX1(FLOAT(WSTRT),AMIN1(FLOAT(WEND),X*WIDTH+WSTRT))
      FPY=AMAX1(FLOAT(HSTRT),AMIN1(FLOAT(HEND),Y*HEIGHT+HSTRT))
C
      IF(NDEV.EQ.'PRIN' .OR. NDEV .EQ. 'DUMB'
     $ .OR. NDEV.EQ.'VT52' .OR. NDEV.EQ.'V100')THEN
         CALL DDA(X1,Y1,FPX,FPY,SYMBOL(LNCHR))
      ELSE IF(NDEV.EQ.'GIGI')THEN
         CALL PUTSTR('V[')
         CALL PUTINT(NINT(FPX))
         CALL PUTSTR(',')
         CALL PUTINT(HEND-NINT(FPY))
         CALL PUTSTR(']')
      ELSE IF(NDEV.EQ.'4010')THEN
         CALL XY4010(NINT(FPX),NINT(FPY),2)
      ENDIF
C
      RETURN
      END
      SUBROUTINE DOMOVE(X,Y)
C..............................................................................
C.     SUBROUTINE DOMOVE PERFORMS A MOVE OF THE PEN.                          .
C..............................................................................
C
      INTEGER WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      REAL X,Y,FPX,FPY
      CHARACTER *4 NDEV
C
      SAVE /GFXA/
      COMMON/GFXA/WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      SAVE /GFXF/
      COMMON/GFXF/FPX,FPY
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
C
      FPX=AMAX1(FLOAT(WSTRT),AMIN1(FLOAT(WEND),X*WIDTH+WSTRT))
      FPY=AMAX1(FLOAT(HSTRT),AMIN1(FLOAT(HEND),Y*HEIGHT+HSTRT))
C
C  FOR VECTOR GRAPHICS DEVICES, AN ACTUAL PEN MOVE MUST BE ISSUED.
C
      IF(NDEV.EQ.'GIGI') THEN
         CALL PUTSTR('P[')
         CALL PUTINT( NINT(FPX) )
         CALL PUTSTR(',')
         CALL PUTINT(HEND-NINT(FPY))
         CALL PUTSTR(']')
C
      ELSE IF(NDEV.EQ.'4010')THEN
         CALL XY4010(NINT(FPX),NINT(FPY),1)
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE DOSTYL(OP)
C..............................................................................
C.     SUBROUTINE DOSTYL SETS THE CHARACTER USED FOR THE LINE STYLE.          .
C..............................................................................
C
      INTEGER OP,LNCHR
      INTEGER ESC
      PARAMETER (ESC = 27)
      CHARACTER *4 NDEV
      CHARACTER TKSTY*16
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
      SAVE /GFXG/
      COMMON/GFXG/LNCHR
C
      DATA TKSTY / '`abcdefghijklmno' /
C
C
      IF(NDEV.EQ.'PRIN' .OR. NDEV .EQ. 'DUMB'
     $   .OR. NDEV.EQ.'VT52' .OR. NDEV.EQ.'V100')THEN
         IF(OP.GT.6)THEN
             LNCHR=1
         ELSE
             LNCHR=OP
         ENDIF
      ELSE IF(NDEV.EQ.'GIGI') THEN
         IF(OP.GT.6)THEN
           LNCHR=1
         ELSE
           LNCHR=OP
         ENDIF
         CALL PUTSTR('W(P')
         CALL PUTINT(LNCHR)
         CALL PUTSTR(')')
      ELSE IF(NDEV.EQ.'4010') THEN
         IF(OP .GT. 16) THEN
           LNCHR = 1
         ELSE
           LNCHR = OP
         ENDIF
         CALL PUTSTR( CHAR(ESC) // TKSTY(LNCHR:LNCHR))
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE ACURSR
C
C  THIS SUBROUTINE SETS THE ALPHA CURSOR TO THE SAME LOCATION AS THE
C  GRAPHIC CURSOR, IF SUCH ACTION IS APPLICABLE TO THE DEVICE.
C  FOR TEKTRONIX 4010-TYPE TERMINALS, THIS IS AUTOMATIC.
C
      INTEGER R,C
      REAL FPX,FPY
      INTEGER WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      CHARACTER *4 NDEV
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
      SAVE /GFXA/
      COMMON/GFXA/WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      SAVE /GFXF/
      COMMON/GFXF/FPX,FPY
C
      IF(NDEV .EQ. 'VT52') THEN
         R=HEND-NINT(FPY)+HSTRT
         C=NINT(FPX)
C
         CALL XYVT52(R,C)
C
      ELSE IF(NDEV .EQ. 'V100') THEN
         R=HEND-NINT(FPY)+HSTRT
         C=NINT(FPX)
C
         CALL XYV100(R,C)
      ELSE IF(NDEV .EQ. 'GIGI') THEN
C   CALCULATE ROW, COLUMN CORRESPONDING TO FRAME PEN X,Y POSITION
         R=INT( (HEND-FPY)/20. ) + 1
         C=INT( (FPX-WSTRT)/9. ) + 1
C
         CALL XYV100(R,C)
      ENDIF
      RETURN
      END
      SUBROUTINE DDA(X1,Y1,X2,Y2,CHR)
C.............................................................................
C.     SUBROUTINE DDA GENERATES THE INTEGER VALUES FOR ALL THE COORDINATES   .
C.     OF A LINE GIVEN THE STARTING AND ENDING COORDINATES.                  .
C.............................................................................
C
      INTEGER STEPS,COL,ROW,I
        REAL X1,X2,Y1,Y2,DX,DY,X,Y,DXSTEP,DYSTEP
      CHARACTER*1 CHR
C
C      ---DETERMINE THE TOTAL CHANGE IN X AND Y---
C
         DX=X2-X1
C
         DY=Y2-Y1
C
C      ---DETERMINE HOW MANY STEPS ARE NECESSARY---
C
         STEPS=MAX1(ABS(DX),ABS(DY))+1
C
C      ---DETERMINE THE X AND Y INCREMENTS---
C
         DXSTEP=DX/FLOAT(STEPS)
C
         DYSTEP=DY/FLOAT(STEPS)
C
C      ---SET THE INITIAL VALUES---
C
         X=X1+0.5
C
         Y=Y1+0.5
C
C      ---DO LOOP TO GENERATE THE LINE---
C
      DO 100 I=1,STEPS
         COL=NINT(X)
         ROW=NINT(Y)
         CALL SETPIX(ROW,COL,CHR)
         X=X+DXSTEP
         Y=Y+DYSTEP
  100  CONTINUE
C
C      ---RETURN---
C
      RETURN
      END
C
C
      SUBROUTINE SETPIX(GVNROW,GVNCOL,CHR)
C.............................................................................
C.     SUBROUTINE SETPIX WILL PLACE A CHARACTER IN THE FRAME BUFFER AT THE   .
C.     PROPER POSITION, OR ON THE SCREEN OF AN ASCII TERMINAL.               .
C.............................................................................
C
      INTEGER WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
     $, GVNROW,GVNCOL,ROW,COL,R,C
      CHARACTER*1 CHR,FRAME
      CHARACTER *4 NDEV
C
      SAVE /GFXA/
      COMMON/GFXA/WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      SAVE /GFXB/
      COMMON/GFXB/FRAME(130,50)
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
C
C
C      ---TEST TO SEE IF POSITION IS IN BOUNDS AND SET TO BORDER IF NOT---
C
      ROW = GVNROW
      COL = GVNCOL
      IF(COL.LT.WSTRT)COL=WSTRT
      IF(COL.GT.WEND)COL=WEND
      IF(ROW.LT.HSTRT)ROW=HSTRT
      IF(ROW.GT.HEND)ROW=HEND
C
C     ---DETERMINE WHICH OUTPUT DEVICE---
C
      IF(NDEV.EQ.'PRIN' .OR. NDEV .EQ. 'DUMB')THEN
C
C     ---IF A PRINTER,GIVE THE DESIRED POSITION THE INTENSITY---
C
      FRAME(COL,ROW)=CHR
C
      ELSE IF(NDEV.EQ.'VT52') THEN
C
C     ---IF A VT52 ---
C
         R=(HEND-ROW)+HSTRT
         C=COL
C
         CALL XYVT52(R,C)
         CALL PUTSTR(CHR)
      ELSE IF(NDEV.EQ.'V100') THEN
C
C     ---IF A VT100 OUTPUT THE SEQUENCE TO SET PIXELS ON THE SCREEN---
C
         R=(HEND-ROW)+HSTRT
         C=COL
C
         CALL XYV100(R,C)
         CALL PUTSTR(CHR)
C
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE XYVT52(R,C)
      INTEGER R,C
      INTEGER ESC
      PARAMETER (ESC = 27)
C  SEND THE VT52 SEQUENCE TO POSITION THE ALPHA CURSOR TO ROW R, COLUMN C
         CALL PUTSTR(CHAR(ESC) // 'Y' // CHAR(R+31) // CHAR(C+31) )
      RETURN
      END
      SUBROUTINE XYV100(R,C)
      INTEGER R,C
      INTEGER ESC
      PARAMETER (ESC = 27)
C  SEND THE VT100 SEQUENCE TO POSITION THE ALPHA CURSOR TO ROW R, COLUMN C
         CALL PUTSTR(CHAR(ESC) // '[')
         CALL PUTINT(R)
         CALL PUTSTR(';')
         CALL PUTINT(C)
         CALL PUTSTR('H')
      RETURN
      END
      SUBROUTINE XY4010(X,Y,OP)
C..........................................................................
C     SUBROUTINE XY4010 CONVERTS AN X,Y COORDINATE INTO THE ASCII GRAPHICS
C     SEQUENCE NEEDED FOR THE 4010.  INCLUDES ELIMINATION OF REDUNDANT CHARS
C     OP IS 1 FOR A MOVE, AND 2 FOR A DRAW COMMAND
      INTEGER X,Y,OP
      LOGICAL ACTION(4)
C  LOX = LOW-ORDER 5 BITS OF X, HOX = HIGH-ORDER 5 BITS OF X, DITTO FOR Y.
      CHARACTER*1 LOX,HOX,LOY,HOY,KPLOX,KPLOY,KPHOX,KPHOY
      SAVE KPLOX,KPLOY,KPHOX,KPHOY
      INTEGER GS
      PARAMETER (GS = 29)
      DATA KPLOX,KPLOY,KPHOX,KPHOY / ' ',' ','A','A' /
C
C  UNPACK THE 10-BIT INTEGERS INTO 5-BIT PIECES WITH SUITABLE HIGH 2 BITS
C  FOR ASCII CODING.
C       LOX = CHAR((X .AND. OCT37) .OR. OCT100)
C       HOX = CHAR((X/32 .AND. OCT37) .OR. OCT40)
C       LOY = CHAR((Y .AND. OCT37) .OR. (OCT100 .OR. OCT40))
C       HOY = CHAR((Y/32 .AND. OCT37) .OR. OCT40)
      LOX = CHAR(MOD(X,32) + 64)
      HOX = CHAR(MOD(X/32,32) + 32)
      LOY = CHAR(MOD(Y,32) + 96)
      HOY = CHAR(MOD(Y/32,32) + 32)
C
C  FIGURE OUT WHICH BYTES NEED TO BE SENT.  SEE 4010 MANUAL FOR RULES.
C  ACTION BIT 0 => LOX MUST BE SENT, BIT 1 => HOX MUST BE SENT, BIT 2 =>
C  LOY MUST BE SENT, AND BIT 3 => HOY MUST BE SENT.
C
      ACTION(1) = .TRUE.
      ACTION(2) = .FALSE.
      ACTION(3) = .FALSE.
      ACTION(4) = .FALSE.
      IF(HOY .NE. KPHOY) THEN
        ACTION(3) = .TRUE.
        ACTION(4) = .TRUE.
      ENDIF
      IF(LOY .NE. KPLOY) ACTION(3) = .TRUE.
      IF(HOX .NE. KPHOX) THEN
        ACTION(2) = .TRUE.
        ACTION(3) = .TRUE.
      ENDIF
C
C  SEND THE BYTES IN THE ORDER <GS>,HOY,LOY,HOX,LOX
C
C  SEND <GS> FOR A MOVE
      IF(OP .EQ. 1) CALL PUTSTR( CHAR(GS) )
C
      IF(ACTION(4)) CALL PUTSTR( HOY)
      IF(ACTION(3)) CALL PUTSTR( LOY)
      IF(ACTION(2)) CALL PUTSTR( HOX)
      CALL PUTSTR( LOX)
C
      KPHOX=HOX
      KPLOX=LOX
      KPHOY=HOY
      KPLOY=LOY
C
      RETURN
      END
C
      SUBROUTINE GGON
C...........................................................................
C     SUBROUTINE GGON PUTS THE GIGI INTO GRAPHIC MODE
C.........................................................................
      INTEGER ESC
      PARAMETER (ESC = 27)
      LOGICAL ONFLAG
      SAVE /GFXGG/
      COMMON/GFXGG/ONFLAG
      IF(.NOT. ONFLAG) THEN
          CALL PUTSTR( CHAR(ESC) // 'Pp')
          ONFLAG=.TRUE.
      ENDIF
      RETURN
      END
      SUBROUTINE GGOFF
C...........................................
C  PUTS GIGI OUT OF GRAPHIC MODE
C...........................................
      INTEGER ESC
      PARAMETER (ESC = 27)
      LOGICAL ONFLAG
      SAVE /GFXGG/
      COMMON/GFXGG/ONFLAG
C  For Unix F77, change '\' to '\\' in the following statement
          CALL PUTSTR( CHAR(ESC) // '\\')
          ONFLAG=.FALSE.
      END
C
      SUBROUTINE CLEAR
C.............................................................................
C.     SUBROUTINE CLEAR SETS THE WHOLE FRAME BUFFER TO A BACKGROUND VALUE    .
C.     OR CLEAR THE SCREEN ON A TERMINAL.
C.............................................................................
C
      INTEGER WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      CHARACTER *4 NDEV
      CHARACTER*1 BKGND,FRAME
C     ---DEFINE CONTROL CODES FOR ESCAPE AND FORMFEED
      INTEGER ESC,FF
      PARAMETER (ESC=27, FF=12)
      INTEGER I,J
C
      SAVE /GFXA/
      COMMON/GFXA/WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      SAVE /GFXB/
      COMMON/GFXB/FRAME(130,50)
      SAVE /GFXDEV/
      COMMON/GFXDEV/NDEV
      REAL TKSTAL
      SAVE /GFXWT/
      COMMON/GFXWT/ TKSTAL
C
C      ---SET THE BACKGROUND VALUE---
C
      PARAMETER (BKGND = ' ')
C
C      ---DETERMINE WHICH OUTPUT DEVICE---
C
        IF(NDEV.EQ.'PRIN' .OR. NDEV .EQ. 'DUMB')THEN
C
C      ---IF A PRINTER,DO LOOP TO SET WHOLE FRAME BUFFER TO BACKGROUND VALUE---
C
           DO 100 I=HSTRT,HEND
C
              DO 200 J=WSTRT,WEND
                 FRAME(J,I)=BKGND
  200         CONTINUE
C
  100      CONTINUE
C
        ELSE IF(NDEV.EQ.'VT52')THEN
C
C      ---IF A VT52 SEND ESCAPE CODE TO CLEAR SCREEN---
C
           CALL PUTSTR( CHAR(ESC) // 'H' // CHAR(ESC) // 'J')
        ELSE IF(NDEV.EQ.'V100')THEN
C
C      ---IF A VT100 OUTPUT THIS TO CLEAR SCREEN---
C
           CALL PUTSTR( CHAR(ESC) // '[2J' )

        ELSE IF(NDEV.EQ.'GIGI')THEN
C
C     ---IF A GIGI, SEND THE SCREEN-EMPTY COMMAND---
C
           CALL PUTSTR('S(E)')
        ELSE IF(NDEV.EQ.'4010')THEN
C
C     ---IF A TEKTRONIX 4010, SEND ESCAPE AND FORMFEED--
C
           CALL PUTSTR( CHAR(ESC) // CHAR(FF))
C  Real Tektronix 4010 storage display needs appx 1 sec of stall here.
           CALL GFWAIT(TKSTAL)
        ENDIF
C
C      ---RETURN---
C
      RETURN
      END
C
      SUBROUTINE DOTRNS(X,Y,NWX,NWY)
C..............................................................................
C.     SUBROUTINE DOTRNS TRANSFORMS A POINT.                                  .
C..............................................................................
C
      REAL X,Y,H,TEMP,NWX,NWY
C
      LOGICAL NEWTRN
      REAL SCLX,SCLY,ANGL,TRNX,TRNY
      SAVE /GFXTRN/
      COMMON/GFXTRN/SCLX,SCLY,ANGL,TRNX,TRNY,NEWTRN
      SAVE /GFXM/
      COMMON/GFXM/H(3,2)
C
      IF(NEWTRN) CALL BLDTRN
C
      TEMP=X*H(1,1)+Y*H(2,1)+H(3,1)
      NWY=X*H(1,2)+Y*H(2,2)+H(3,2)
      NWX=TEMP
C
      RETURN
      END
C
      SUBROUTINE BLDTRN
C..............................................................................
C.     SUBROUTINE BLDTRN BUILDS THE IMAGE TRANSFORMATION MATRIX.              .
C..............................................................................
C
      REAL SCLX,SCLY,ANGL,TRNX,TRNY
C
      LOGICAL NEWTRN
      SAVE /GFXTRN/
      COMMON/GFXTRN/SCLX,SCLY,ANGL,TRNX,TRNY,NEWTRN
C
      CALL IDMTRX
      CALL MLTSCL(SCLX,SCLY)
      CALL MLTROT(ANGL)
      CALL MLTTRN(TRNX,TRNY)
      NEWTRN=.FALSE.
C
      RETURN
      END
C
C
C
      SUBROUTINE IDMTRX
C.............................................................................
C.     SUBROUTINE IDMTRX CREATES THE IDENTITY TRANSFORMATION MATRIX.         .
C.............................................................................
C
      REAL H
C
      INTEGER I,J
      SAVE /GFXM/
      COMMON/GFXM/H(3,2)
C
      DO 100 I=1,3
        DO 200 J=1,2
          IF(I.EQ.J)THEN
           H(I,J)=1.0
          ELSE
           H(I,J)=0.0
          ENDIF
  200   CONTINUE
  100 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE MLTSCL(SX,SY)
C.............................................................................
C.     SUBROUTINE MLTSCL MULTIPLIES THE TRANSFORMATION MATRIX BY A SCALE     .
C.      TRANSFORMATION.                                                      .
C.............................................................................
C
      REAL SX,SY,H
      INTEGER I
C
      SAVE /GFXM/
      COMMON/GFXM/H(3,2)
C
      DO 100 I=1,3
        H(I,1)=H(I,1)*SX
        H(I,2)=H(I,2)*SY
  100 CONTINUE
C
      RETURN
      END
C
      SUBROUTINE MLTTRN(TX,TY)
C.............................................................................
C.   SUBROUTINE MLTTRN MULTIPLIES THE TRANSFORMATION MATRIX BY A TRANSLATION..
C.............................................................................
C
      REAL TX,TY,H
C
      SAVE /GFXM/
      COMMON/GFXM/H(3,2)
C
      H(3,1)=H(3,1)+TX
      H(3,2)=H(3,2)+TY
C
      RETURN
      END
C
      SUBROUTINE MLTROT(A)
C.............................................................................
C.     SUBROUTINE MLTROT MULTIPLIES THE TRANSFORMATION MATRIX BY A ROTATION. .
C.............................................................................
C
      REAL A,H,C,S,TEMP
      INTEGER I
C
      SAVE /GFXM/
      COMMON/GFXM/H(3,2)
C
      C=COS(A/57.3)
      S=SIN(A/57.3)
C
      DO 100 I=1,3
        TEMP=H(I,1)*C-H(I,2)*S
        H(I,2)=H(I,1)*S+H(I,2)*C
        H(I,1)=TEMP
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE DISPLY
C...........................................................................
C.     SUBROUTINE DISPLY DISPLAYS THE FRAME BUFFER.  APPLICABLE TO NON-    .
C.     SCREEN ADDRESSABLE DEVICES.                                         .
C...........................................................................
C
      INTEGER WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT,COL,ROW,MAX,IHSTRT
      CHARACTER*1 FRAME,BKGND
C
      SAVE /GFXA/
      COMMON/GFXA/WSTRT,HSTRT,WEND,HEND,WIDTH,HEIGHT
      SAVE /GFXB/
      COMMON/GFXB/FRAME(130,50)
C
      INTEGER GFXU,ERRU,INPTU
      SAVE /GFXIO/
      COMMON/GFXIO/ GFXU,ERRU,INPTU
C
      PARAMETER (BKGND = ' ')
C
C      ---POSITION AT TOP OF PAGE---
C
      WRITE(GFXU,5)
    5   FORMAT(1H1)
C
      IHSTRT=HSTRT-1
C
C      ---DO LOOP TO OUTPUT THE FRAME BUFFER---
C
      DO 100 ROW=HEND,HSTRT,-1
C
C      ---ELIMINATE TRAILING BLANKS---
C
          MAX=WEND
C
   20    IF ((FRAME(MAX,ROW).NE.BKGND) .OR. (MAX.EQ.WSTRT)) THEN
C
             WRITE(GFXU,10)(FRAME(COL,IHSTRT+ROW),COL=WSTRT,MAX)
C 
   10        FORMAT(1X,130A1)
C
         ELSE
C
             MAX=MAX-1
C
             GOTO 20
C
         ENDIF
C
  100  CONTINUE
C
C      ---RETURN---
C
      RETURN
      END
C
C
C
      SUBROUTINE GFXERR(MESG)
C  ISSUES ERROR MESSAGE TO ERROR-OUTPUT UNIT
      CHARACTER *(*) MESG
      INTEGER GFXU,ERRU,INPTU
      SAVE /GFXIO/
      COMMON/GFXIO/ GFXU,ERRU,INPTU
      WRITE(ERRU,*) 'CORE ERROR: ',MESG
      RETURN
      END

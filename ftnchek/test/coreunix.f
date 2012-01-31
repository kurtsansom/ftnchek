C File COREUNIX.F:
C
C  Contains system-dependent part of CORE graphics package.  This file for
C  Linux operating system, gfortran compiler.
C  Append to CORE.F when compiling to make complete object module.
C
C  Routines included:
C      BLOCK DATA DEFLTS  Contains default values for certain variables.
C      SUBROUTINE PUTSTR(S)    Prints string with no CR/LF
C      SUBROUTINE PUTINT(N)    Prints integer with no blanks or CR/LF
C      SUBROUTINE PUTNUM(X,FRMT) Prints number with given format, no CR/LF
C      SUBROUTINE GETSPD(IUNIT,IBAUD) Gets terminal speed from user.
C      SUBROUTINE RNDIZE(NSEED) Randomizes random no. gen. for games, simulatns
C      FUNCTION RAN(NSEED) Random number generator for compatibility.
C
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
        BLOCK DATA DEFLTS
C
C Sets various default numbers.  Placed here since the standard numbers
C vary under different operating systems.  User can change I/O unit numbers
C from the defaults by calling SETGFU, SETERU, and SETINU.
C
C  GFXU is graphic output unit for display.
C  ERRU error-logging output unit for error messages.
C  INPTU is unit for interactive input from user.  (Not used by CORE routines
C    except GETSPD.)
C
        INTEGER GFXU,ERRU,INPTU
        SAVE /GFXIO/
        COMMON /GFXIO/ GFXU,ERRU,INPTU
C
C  TKSTAL is the amount of time (in seconds) needed to stall after clearing
C  the screen for a Tektronix terminal.  Most Tek emulators need no stall.
C  A genuine 4010 storage display needs 1.0 second to avoid loss of graphic
C  data.
      REAL TKSTAL
      SAVE /GFXWT/
      COMMON/GFXWT/ TKSTAL
C
C  DFBAUD is the default baud rate to assume if it is not provided.
      INTEGER DFBAUD
      SAVE /GFBAUD/
      COMMON /GFBAUD/ DFBAUD
C
C  These are the standard BSD Unix unit numbers for stdout, stderr, stdin
        DATA GFXU /6/, ERRU /0/, INPTU /5/

C  Assume we have not got the real thing.
      DATA TKSTAL / 0.0 /
C
C DFBAUD should be set to the baud rate of most terminals on the system.
      DATA DFBAUD /9600/
C
      END
C
        SUBROUTINE PUTSTR(S)
C
C  PUTS OUT A STRING OF 1 OR MORE CHARACTERS WITH NO CARRIAGE-RETURN
C  OR LINEFEED.  OUTPUT IS TO GRAPHIC-OUTPUT UNIT.
C  BSD UNIX F77 VERSION.
        INTEGER GFXU,ERRU,INPTU
        SAVE /GFXIO/
        COMMON /GFXIO/ GFXU,ERRU,INPTU
        CHARACTER *(*) S
        CHARACTER *13 IFORM
C  Format IFORM will look like  (A<len>,$)  at run time, where <len> is the
C  length of string S.  The final $ is Unix-f77 to suppress newline.
        DATA IFORM / '(A0000,$)' /
        IF( LEN(S) .EQ. 0 ) RETURN
C  here we replace the 0000 by length of string.
        WRITE(IFORM(3:6),100) LEN(S)
100     FORMAT(I4)
C  here we use the customized format to print the string.
        WRITE(GFXU,IFORM) S
        RETURN
        END
     
        SUBROUTINE PUTINT(X)
C.............................................................................
C.     SUBROUTINE PUTINT OUTPUTS AN INTEGER ARGUMENT BETWEEN 1 AND 999 WITH
C.     NO LEADING OR TRAILING BLANKS.
C     UNIX F77 VERSION.
C.............................................................................
     
        INTEGER X
        INTEGER GFXU,ERRU,INPTU
        SAVE /GFXIO/
        COMMON /GFXIO/ GFXU,ERRU,INPTU
C      ---TEST IF NUMBER IS 1, 2, OR 3 DIGITS---
        IF(X.LT.10)THEN
           WRITE(GFXU,10)X
   10      FORMAT(I1,$)
        ELSE IF(X .LT. 100) THEN
           WRITE(GFXU,20)X
   20      FORMAT(I2,$)
        ELSE
           WRITE(GFXU,30)X
   30      FORMAT(I3,$)
        ENDIF
        RETURN
        END
        SUBROUTINE PUTNUM(X,FRMT)
C
C  PRINTS THE REAL NUMBER X WITH THE GIVEN FORMAT, SUPPRESSING CARRIAGE
C  RETURN AND LINE FEED.  FRMT SHOULD BE A FORMAT EDIT DESCRIPTOR,
C  WITHOUT THE PARENTHESES, E.G. FRMT = 'F5.1'   LENGTH OF FRMT MUST
C  NOT EXCEED 24 CHARACTERS.
C
        REAL X
        CHARACTER *(*) FRMT
        CHARACTER *28 F
        INTEGER GFXU,ERRU,INPTU
        SAVE /GFXIO/
        COMMON /GFXIO/ GFXU,ERRU,INPTU
        F = '(' // FRMT // ',$)'
        WRITE(GFXU,F) X
        END
C
C GETSPD SUBROUTINE FOR CORE GRAPHICS PACKAGE FOR NON-VMS SYSTEMS.
C
C  Gets terminal speed.  Here we just return an arbitrary value.
      SUBROUTINE GETSPD(IUNIT,IBAUD)
      IBAUD = 9600
      END
C
C  RANDOM NUMBER GENERATORS WORK DIFFERENTLY ON DIFFERENT UNIX SYSTEMS.
C  THEREFORE, TWO VERSIONS ARE INCLUDED HERE.  ONE IS COMMENTED OUT.  IF
C  THE VERSION IN EFFECT DOES NOT WORK, TRY DELETING IT AND UNCOMMENTING
C  THE OTHER VERSION.  Version 1 is by Mark Shapiro, CSU-Fullerton.
C     
      SUBROUTINE RNDIZE(NSEED)
C
C  RANDOMIZES THE RANDOM NUMBER SEED SO THAT EACH RUN OF PROGRAM IS DIFFERENT.
C
C  UNIX VERSION: USES SYSTEM TIME FUNCTION TO RANDOMIZE THE RANDOM
C  NUMBER GENERATOR.
C
      INTEGER TIME
      NSEED = TIME()
C version 1:
      CALL SRAND(NSEED)
C end of version 1.
C version 2:
CC      CALL RANDOM(NSEED)
C end of version 2.
      RETURN
      END
      FUNCTION RAN(NSEED)
C
C  RANDOM NUMBER GENERATOR.  RETURNS RANDOM VALUE FROM 0.0 TO 1.0
C  PLACED HERE SINCE DIFFERENT SYSTEMS HAVE DIFFERENT
C  NAMES FOR THIS FUNCTION.  RAN IS THE NAME OF THE VMS RANDOM NUMBER
C  FUNCTION.
C
C  BSD Unix version uses the newer function RANDOM, not bad old RAND.
C
C version 1:
      DOUBLE PRECISION RAND
      RAN = RAND()
C end of version 1.
C version 2:
CC      RAN = RANDOM(0)
C end of version 2.
      RETURN
      END

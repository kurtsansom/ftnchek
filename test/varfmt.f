

      PROGRAM VARIABLE_FORMAT

C Variable format sample for ftnchek regression tests
C
C Author: Stuart G. Mentzer (Stuart_Mentzer@objexx.com)
C
C Language: FORTRAN 77 with VMS-style Variable Format Support Extension
C
C Compiler: Tested with Intel Fortran for Windows 9.0
C
C Build: ifort -nologo -f77rtl -Qansi_alias- -warn:all -nodebug VARIABLE_FORMAT.for

      INTEGER N ! Variable counter
      INTEGER NEXT ! Function
      REAL X ! Floating point counter

      N = 3
      X = 3.3

C Simple repeat count
      PRINT 101, 1, 2, 3
 101  FORMAT(<N>I3)

C Simple repeat count using floating point expression
      PRINT 102, 1, 2, 3 ! Legal: Automatic conversion to integer
 102  FORMAT(<X>I3)

C Function repeat count
      PRINT 103, 1, 2, 3
 103  FORMAT(<NEXT(2)>I3)

C Simple repeat count and field width
      PRINT 104, 1, 2, 3
 104  FORMAT(<N>I<N-1>)

C More complicated repeat count and field width
      PRINT 105,
     &  1, 2, 3, 11.1, 22.2, 33.3, 44.4
 105  FORMAT(<N>I<N-1>,<NEXT(N)>F<N+2>.<N-2>)

C Illegal use with Hollerith
      PRINT 106
 106  FORMAT(3HCAT)             ! Legal
      PRINT 107
 107  FORMAT(<N>HCAT)           ! Illegal: Shouldn't compile

      END PROGRAM


      INTEGER FUNCTION NEXT( I )

      INTEGER I

      NEXT = I + 1

      RETURN
      END

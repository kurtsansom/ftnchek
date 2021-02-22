C entry4.f: example program with a subroutine having different entry
C points with different interfaces.  This program is F90 standard conforming.
C It is not F77 standard conforming because the concatenation of an
C assumed-length character variable is only allowed in an assignment statement.
      SUBROUTINE S1
      CHARACTER*(*) C
      PRINT *, 'hello from S1'
      RETURN
      ENTRY S2(C)
      PRINT *, C // ' from S2'
      RETURN
      END

      PROGRAM P
      CALL S1
      CALL S2('goodbye')
      END

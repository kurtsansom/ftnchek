C entry3.f: defines result variable F_RESULT for all 3 entry points.
C This program is F90 standard conforming.
       REAL FUNCTION F2 ( X ) result(F_RESULT)
       F_RESULT = 2.0 * X 
       RETURN 

       ENTRY F3 ( X, Y )  result(F_RESULT)
       F_RESULT = 3.0 * X * Y
       RETURN 

       ENTRY FHALF ( X )  result(F_RESULT)
       F_RESULT = X / 2.0 
       RETURN 
       END 

      PROGRAM P
      REAL Y
      Y = F2(2.0)
      PRINT *, Y
      Y = F3(2.0,3.0)
      PRINT *, Y
      Y = FHALF(2.0)
      PRINT *, Y
      END

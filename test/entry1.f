C entry1.f: example of a function with entry points, not all having
C the same interface.  This program is F77 standard conforming.
       REAL FUNCTION F2 ( X ) 
       F2 = 2.0 * X 
       RETURN 

       ENTRY F3 ( X, Y ) 
       F3 = 3.0 * X * Y
       RETURN 

       ENTRY FHALF ( X ) 
       FHALF = X / 2.0 
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
